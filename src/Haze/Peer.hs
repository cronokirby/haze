{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{- |
Description: Contains types and functions related to peer communication.
-}
module Haze.Peer
    ( BlockInfo(..)
    , Message(..)
    , encodeMessage
    , parseMessage
    , ParseCallBack
    , firstParseCallBack
    , parseMessages
    )
where

import           Relude

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( async
                                                , waitAnyCatchCancel
                                                )
import           Control.Concurrent.STM.TBQueue ( readTBQueue
                                                , writeTBQueue
                                                )
import           Control.Exception.Safe         ( Exception
                                                , MonadThrow
                                                , throw
                                                )
import           Data.Attoparsec.ByteString    as AP
import           Data.Array                     ( (!)
                                                , assocs
                                                , bounds
                                                )
import qualified Data.ByteString               as BS
import           Data.Ix                        ( inRange )
import qualified Data.Set                      as Set
import           Data.Time.Clock                ( getCurrentTime )
import           Network.Socket                 ( PortNumber
                                                , Socket
                                                )
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )

import           Data.RateWindow                ( addDownload )
import           Haze.Bits                      ( encodeIntegralN
                                                , parseInt
                                                , parse16
                                                )
import           Haze.Messaging                 ( PeerToWriter(..)
                                                , SelectorToPeer(..)
                                                , WriterToPeer(..)
                                                )
import           Haze.PeerInfo                  ( PeerHandle(..), PeerFriendship(..) )

import           Haze.PieceBuffer               ( BlockInfo(..)
                                                , BlockIndex(..)
                                                , makeBlockInfo
                                                , HasPieceBuffer(..)
                                                , nextBlockM
                                                , writeBlockM
                                                )
import           Haze.Tracker                   (TrackStatus(..))


-- | The messages sent between peers in a torrent
data Message
    -- | Used to keep the connection open (think PING
    = KeepAlive
    -- | The sender has choked the receiver
    | Choke
    -- | The sender has unchoked the receiver
    | UnChoke
    -- | The sender is now interested in the receiver
    | Interested
    -- | The sender is no longer interested in the receiver
    | UnInterested
    -- | The sender claims to have piece #index
    | Have !Int
    {- | The sender is requesting a block in a certain piece.

    The first Int represents which piece is being asked for. The 2nd
    the zero base byte offset within the piece. And the 3rd the length.
    -}
    | Request !BlockInfo
    {- | The sender is fulfilling a block request.

    The first 2 bytes represent the piece index and byte offset.
    The final part is the actual bytes constituting the block.
    -}
    | RecvBlock !BlockIndex !ByteString
    -- | The send is no longer requesting a block
    | Cancel !BlockInfo
    -- | The port this peer's DHT is listening on
    | Port PortNumber
    deriving (Eq, Show)


-- | Encodes a message as a bytestring, ready to send
encodeMessage :: Message -> ByteString
encodeMessage m = case m of
    KeepAlive     -> BS.pack $ encInt 0
    Choke         -> BS.pack $ encInt 1 ++ [0]
    UnChoke       -> BS.pack $ encInt 1 ++ [1]
    Interested    -> BS.pack $ encInt 1 ++ [2]
    UnInterested  -> BS.pack $ encInt 1 ++ [3]
    Have    i     -> BS.pack $ encInt 5 ++ [4] ++ encInt i
    Request block -> BS.pack $ encInt 13 ++ [6] ++ encBlock block
    RecvBlock (BlockIndex a b) block ->
        let len = BS.length block
        in  BS.pack (encInt (len + 9) ++ [7] ++ encInt a ++ encInt b) <> block
    Cancel block -> BS.pack $ encInt 13 ++ [8] ++ encBlock block
    Port p -> BS.pack $ encInt 3 ++ [9] ++ drop 2 (encInt (fromIntegral p))
  where
    encInt :: Int -> [Word8]
    encInt = encodeIntegralN 4
    encBlock :: BlockInfo -> [Word8]
    encBlock (BlockInfo (BlockIndex a b) c) = foldMap encInt [a, b, c]


{- | Parse a message encoded as as string of bytes

Takes the current size of the block we're expected to receive, if any.
If no block is expected, but a Receive message is parsed, then this
parser will fail.
-}
parseMessage :: AP.Parser Message
parseMessage = do
    len <- parseInt
    if len == 0 then return KeepAlive else AP.anyWord8 >>= parseID len
  where
    parseID :: Int -> Word8 -> AP.Parser Message
    parseID 1  0 = return Choke
    parseID 1  1 = return UnChoke
    parseID 1  2 = return Interested
    parseID 1  3 = return UnInterested
    parseID 5  4 = Have <$> parseInt
    parseID 13 6 = Request <$> liftA3 makeBlockInfo parseInt parseInt parseInt
    parseID 13 8 = Cancel <$> liftA3 makeBlockInfo parseInt parseInt parseInt
    parseID 3  9 = Port <$> parse16
    parseID ln 7 = do
        index <- parseInt
        begin <- parseInt
        let blockLen = ln - 9
        RecvBlock (BlockIndex index begin) <$> AP.take blockLen
    parseID _ _ = fail "Unrecognised ID, or bad length"


newtype ParseCallBack = ParseCallBack (ByteString -> AP.Result Message)

-- | The callback that should be used at the beginning of parsing
firstParseCallBack :: ParseCallBack
firstParseCallBack = ParseCallBack (AP.parse parseMessage)

{- | Attempt to parse all available messages from a bytestring

Returns Nothing if parsing failed at some point.
-}
parseMessages :: ParseCallBack -> ByteString -> Maybe ([Message], ParseCallBack)
parseMessages callback bytes = do
    (messages, cb) <- gotoPartial callback bytes []
    -- We want to make sure and output messages in the order we received them
    return (reverse messages, cb)
  where
    gotoPartial (ParseCallBack cb) bs acc = case cb bs of
        AP.Fail{}   -> Nothing
        Partial f   -> Just (acc, ParseCallBack f)
        Done next m -> gotoPartial firstParseCallBack next (m : acc)


-- | Represents the state of p2p communication
data PeerState = PeerState
    { peerPieces :: !(Set Int) -- | The set of pieces this peer has
    {- | What piece we might have already requested

    We choose to download pieces reactively, so we need to know
    if we've already downloaded a piece to avoid jumping on another one.
    -}
    , peerRequested :: !(Maybe Int)
    -- | A set of blocks we shouldn't send off
    , peerShouldCancel :: !(Set BlockIndex)
    -- | Used to cancel if no messages are received
    , peerKeepAlive :: !Bool
    -- | The socket connection for this peer
    , peerSocket :: !Socket
    }

-- | The peer state at the start of communication
initialPeerState :: Socket -> PeerState
initialPeerState =
    PeerState Set.empty Nothing Set.empty True


{- | The information needed in a peer computation

As opposed to PeerMState, the variables here can be shared with other
sections of the code.
-}
data PeerMInfo = PeerMInfo
    { peerMState :: !(TVar PeerState) -- ^ The local state
    -- | A handle to the information shared with us
    , peerMHandle :: !PeerHandle
    }

-- | Represents computations for a peer
newtype PeerM a = PeerM (ReaderT PeerMInfo IO a)
        deriving (Functor, Applicative, Monad,
                  MonadIO, MonadThrow)

instance MonadReader PeerHandle PeerM where
    ask = PeerM (asks peerMHandle)
    local f (PeerM m ) = 
        PeerM (local (\r -> r { peerMHandle = f (peerMHandle r)}) m)


instance MonadState PeerState PeerM where
    state f = do
        stRef <- PeerM (asks peerMState)
        atomically $ do
            st <- readTVar stRef
            let (a, st') = f st
            writeTVar stRef st'
            return a

instance HasPieceBuffer PeerM where
    getPieceBuffer = asks handleBuffer

-- | Run a peer computation given the initial information it needs
runPeerM :: PeerMInfo -> PeerM a -> IO a
runPeerM r (PeerM rdr) = runReaderT rdr r

-- | Increment the upload count of our Status
incrementTrackUp :: Integral a => a -> PeerM ()
incrementTrackUp n = do
    status <- asks handleStatus
    atomically $ modifyTVar' status increment
  where
    increment t@TrackStatus{..} = t { trackUp = trackUp + fromIntegral n}

-- | Increment the download count of our Status
incrementTrackDown :: Integral a => a -> PeerM ()
incrementTrackDown n = do
    status <- asks handleStatus
    atomically $ modifyTVar' status increment
  where
    increment t@TrackStatus{..} = t { trackDown = trackDown + fromIntegral n}

-- | Modify our friendship atomically
modifyFriendship :: (PeerFriendship -> PeerFriendship) -> PeerM ()
modifyFriendship f = do
    friendShip <- asks handleFriendship
    atomically (modifyTVar' friendShip f)

-- | Get some information about our friendship
askFriendship :: (PeerFriendship -> a) -> PeerM a
askFriendship f = f <$> (asks handleFriendship >>= readTVarIO)


-- | Represents the different types of exceptions with a peer
data PeerException
    -- | The peer committed a fatal mistake in communication
    = PeerMistakeException
    deriving (Show)

instance Exception PeerException

{- | Cancel the connection with the peer.

This simply throws an exception. And any cleanup should be done
above.
-}
cancel :: PeerM ()
cancel = throw PeerMistakeException


-- | Add a piece locally, and increment it's global count.
addPiece :: Int -> PeerM ()
addPiece piece = do
    pieces <- asks handlePieces
    unless (inRange (bounds pieces) piece) cancel
    atomically $ modifyTVar' (pieces ! piece) (+ 1)
    modify (addLocalPiece piece)
  where
    addLocalPiece piece' ps =
        let pieces = peerPieces ps
        in  ps { peerPieces = Set.insert piece' pieces }

-- | Send a message to the peer connection
sendMessage :: Message -> PeerM ()
sendMessage msg = do
    socket <- gets peerSocket
    let bytes = encodeMessage msg
    incrementTrackUp (BS.length bytes)
    liftIO $ sendAll socket bytes

-- | Send a message to the writer
sendToWriter :: PeerToWriter -> PeerM ()
sendToWriter msg = do
    q <- asks handleToWriter
    atomically $ writeTBQueue q msg


-- | Modify our state based on a message, and send back a reply
reactToMessage :: Message -> PeerM ()
reactToMessage msg = case msg of
    KeepAlive -> modify (\ps -> ps { peerKeepAlive = True })
    Choke     -> modifyFriendship (\f -> f { peerIsChoking = True })
    UnChoke   -> do
        modifyFriendship (\f -> f { peerIsChoking = False })
        requestRarestPiece
    Interested   -> modifyFriendship (\f -> f { peerIsInterested = True })
    UnInterested -> modifyFriendship (\f -> f { peerIsInterested = False })
    Have piece   -> do
        addPiece piece
        whenJustM getRarestPiece $ \next -> do
            sendMessage Interested
            requested <- gets peerRequested
            choking   <- askFriendship peerIsChoking
            case (choking, requested) of
                (False, Nothing) -> request next
                (_    , _      ) -> return ()
    Request info -> do
        me <- asks handlePeer
        let amChoking = askFriendship peerAmChoking
        unlessM amChoking (sendToWriter (PieceRequest me info))
    RecvBlock index bytes -> do
        writeBlockM index bytes
        sendToWriter PieceBufferWritten
        whenJustM (gets peerRequested) request
    Cancel (BlockInfo index _) -> do
        toCancel <- gets peerShouldCancel
        let toCancel' = Set.insert index toCancel
        modify (\ps -> ps { peerShouldCancel = toCancel' })
    Port _ -> return ()

-- | React to messages sent by the writer
reactToWriter :: WriterToPeer -> PeerM ()
reactToWriter msg = case msg of
    PieceFulfilled index bytes -> do
        shouldCancel <- gets peerShouldCancel
        unless (Set.member index shouldCancel)
            $ sendMessage (RecvBlock index bytes)
    PieceAcquired piece -> do
        sendMessage (Have piece)
        requested <- gets peerRequested
        choking   <- askFriendship peerIsChoking
        when (not choking && isNothing requested) requestRarestPiece

-- | Loop and react to messages sent by the writer
writerLoop :: PeerM ()
writerLoop = forever $ do
    chan <- asks handleFromWriter
    msg  <- atomically $ readTBQueue chan
    reactToWriter msg


-- | React to messages sent from the manager
reactToSelector :: SelectorToPeer -> PeerM ()
reactToSelector m = case m of
    PeerChoke -> do
        modifyFriendship (\f -> f { peerAmChoking = True })
        sendMessage Choke
    PeerUnchoke -> do
        modifyFriendship (\f -> f { peerAmChoking = False })
        sendMessage UnChoke
    PeerWatchForInterest -> return ()

-- | A loop handling messages from the manager
selectorLoop :: PeerM ()
selectorLoop = forever $ do
    chan <- asks handleFromSelector
    msg  <- atomically $ readTBQueue chan
    reactToSelector msg


-- | Get the rarest piece that the peer claims to have, and that we don't
getRarestPiece :: PeerM (Maybe Int)
getRarestPiece = do
    theirPieces <- gets peerPieces
    ourPieces   <- asks handleOurPieces >>= readTVarIO
    pieceArr    <- asks handlePieces
    let wantedPiece (i, _) =
            Set.member i (Set.difference theirPieces ourPieces)
    counts <- traverse getCount $ filter wantedPiece (assocs pieceArr)
    -- TODO: randomise this somewhat
    return . fmap fst . viaNonEmpty head $ sortOn snd counts
    where getCount (i, var) = (,) i <$> readTVarIO var

{- | Send a request for a particular piece.

This will sometimes do nothing if no further blocks are available for that
piece. The time to switch to requesting a different piece is when
we send a have message to the peer.
-}
request :: Int -> PeerM ()
request piece = nextBlockM piece >>= \case
    Just info -> do
        modify (\ps -> ps { peerRequested = Just piece })
        sendMessage (Request info)
    Nothing -> modify (\ps -> ps { peerRequested = Nothing })

-- | Try and request the rarest piece
requestRarestPiece :: PeerM ()
requestRarestPiece = whenJustM getRarestPiece request


{- | A loop for a process that will kill the peer if messages aren't received

The process reading messages from a socket should set `peerKeepAlive`
to `True` when a message is received, to avoid this process cancelling.
-}
checkKeepAliveLoop :: PeerM ()
checkKeepAliveLoop = do
    liftIO . threadDelay $ 2 * 60 * 1_000_000
    unlessM (gets peerKeepAlive) $ do
        modify (\ps -> ps { peerKeepAlive = False })
        checkKeepAliveLoop
    cancel

{- | A loop for a process that sends a keep alive message every minute.

This is needed to maintain a connection with the peer.
-}
sendKeepAliveLoop :: PeerM ()
sendKeepAliveLoop = do
    liftIO . threadDelay $ 60 * 1_000_000
    sendMessage KeepAlive

{- | A loop for a process that will receive, parse, and react to messages.
-}
recvLoop :: ParseCallBack -> PeerM ()
recvLoop cb = do
    socket <- gets peerSocket
    bytes  <- liftIO $ recv socket 1024
    dlRate <- asks handleDLRate
    now    <- liftIO getCurrentTime
    let byteCount = BS.length bytes
        shiftRates = addDownload byteCount now
    atomically $ modifyTVar' dlRate shiftRates
    incrementTrackDown byteCount
    case parseMessages cb bytes of
        Nothing          -> cancel
        Just (msgs, cb') -> do
            forM_ msgs reactToMessage
            recvLoop cb'


{- | Start the tree of processes given the initial information a peer needs.

This will start and wait on 5 processes, one waiting on the socket,
one keeping the socket alive by sending heartbeats,
one waiting to receive from the piecewriter, one from the selector,
and one waiting to kill the connection if it gets stale.
-}
startPeer :: PeerM ()
startPeer = do
    r <- PeerM ask
    let startAsync = liftIO . async . runPeerM r
    checkAlive <- startAsync checkKeepAliveLoop
    stayAlive <- startAsync sendKeepAliveLoop
    socket    <- startAsync (recvLoop firstParseCallBack)
    selector   <- startAsync selectorLoop
    writer    <- startAsync writerLoop
    void . liftIO $ waitAnyCatchCancel 
        [checkAlive, stayAlive, socket, selector, writer]
