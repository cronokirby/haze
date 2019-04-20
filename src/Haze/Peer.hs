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
    , PeerInfo
    , makePeerInfo
    , PeerM
    , runPeerM
    , startPeer
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
import qualified Data.Bits                     as Bits
import qualified Data.ByteString               as BS
import qualified Data.IntMap                   as IntMap
import           Data.Ix                        ( inRange )
import qualified Data.Set                      as Set
import           Data.Time.Clock                ( getCurrentTime )
import           Network.Socket                 ( PortNumber
                                                , Socket
                                                )
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )
import qualified Text.Show

import           Data.RateWindow                ( addDownload )
import           Control.Logger                 ( HasLogger(..)
                                                , LoggerHandle
                                                , Importance(..)
                                                , (.=)
                                                , log
                                                )
import           Haze.Bits                      ( encodeIntegralN
                                                , parseInt
                                                , parse16
                                                )
import           Haze.Messaging                 ( PeerToWriter(..)
                                                , SelectorToPeer(..)
                                                , PeerToSelector(..)
                                                , WriterToPeer(..)
                                                )
import           Haze.PeerInfo                  ( PeerHandle(..)
                                                , PeerFriendship(..)
                                                )

import           Haze.PieceBuffer               ( BlockInfo(..)
                                                , BlockIndex(..)
                                                , makeBlockInfo
                                                , HasPieceBuffer(..)
                                                , nextBlockM
                                                , writeBlockM
                                                )
import           Haze.Tracker                   (Peer, TrackStatus(..))


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
    {- | The sender claims to have a set of pieces

    This should only be sent as the first message, and
    should be ignored in all other cases.
    -}
    | BitField !(Set Int)
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
encodeMessage :: Int -> Message -> ByteString
encodeMessage pieceCount m = case m of
    KeepAlive     -> BS.pack $ encInt 0
    Choke         -> BS.pack $ encInt 1 ++ [0]
    UnChoke       -> BS.pack $ encInt 1 ++ [1]
    Interested    -> BS.pack $ encInt 1 ++ [2]
    UnInterested  -> BS.pack $ encInt 1 ++ [3]
    Have    i     -> BS.pack $ encInt 5 ++ [4] ++ encInt i
    BitField s    -> 
        let groupCount = div (pieceCount -1) 8 + 1
            bitMap = IntMap.fromList (zip [0..] (replicate groupCount 0))
            set x b = Bits.setBit b (7 - mod x 8)
            added = foldr (\x acc -> IntMap.adjust (set x) (x `div` 8) acc) bitMap s
        in BS.pack $ encInt (groupCount + 1) ++ [5] ++ IntMap.elems added
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
    parseID ln 5 = do
        bitSets <- BS.unpack <$> AP.take (ln - 1)
        let hasIndex w i = Bits.testBit w (7 - i)
            tests = map (\w -> filter (hasIndex w) [0..7]) bitSets
            pieceGroups = zipWith (\start -> map (+start)) [0,8..] tests
        return . BitField . Set.fromList  $ concat pieceGroups
    parseID 13 6 = Request <$> liftA3 makeBlockInfo parseInt parseInt parseInt
    parseID 13 8 = Cancel <$> liftA3 makeBlockInfo parseInt parseInt parseInt
    parseID 3  9 = Port <$> parse16
    parseID ln 7 = do
        index <- parseInt
        begin <- parseInt
        let blockLen = ln - 9
        RecvBlock (BlockIndex index begin) <$> AP.take blockLen
    parseID _ _ = fail "Unrecognised ID, or bad length"

-- | A wrapper around message with a nicer show instance
newtype PrettyMessage = PrettyMessage Message

instance Text.Show.Show PrettyMessage where
    show (PrettyMessage m) = case m of
        RecvBlock i _ -> show (RecvBlock i "<bytes>")
        msg           -> show msg


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

{-
The logic inside the peer is quite complicated, so here's a
section dedicated to try and explain it.

The peer logic can be described by a finite state machine,
where we have a finite set of states the peer can be in,
and the peer will transition between states in the following situations:

- Receiving a message from our partner
- Receiving a message from the piece-writer
- Receiving a message from the selector

When looking specifically at the states the peer might take,
another useful point of view is that of "reactive variables".
The idea is that a specific piece of state can be seen as a pure function
of other changing variables, and its current value depends on the
current value of the other variables.

Let's look at these variables:

amInterested: Whether or not we're interested in a piece the peer has
amInterested = notEmpty (theirPieces - myPieces)

currentPieceRequest: The current piece we're requesting
currentPieceRequest = currentPieceRequest or rarestPiece

After changing our interest in a peer, as soon as they unchoke us,
we set our current piece request to their rarest piece. We download
blocks as long as we can. Our current piece will be cleared if we get
choked. If the piece writer informs us that our current piece has been saved,
then we can choose another current piece.
-}

{- | The information needed in a peer computation

As opposed to PeerMState, the variables here can be shared with other
sections of the code.
-}
data PeerInfo = PeerInfo
    { peerPeer :: !Peer -- ^ the peer we're connected to
    -- | The set of pieces this peer has
    , peerPieces :: !(TVar (Set Int))
    -- | The piece we're currently downloading
    , peerRequested :: !(TVar (Maybe Int))
    {- | A set of blocks we shouldn't send off

    It's necessary to keep track of this since we request
    pieces asynchronously from the piece writer.
    -}
    , peerShouldCancel :: !(TVar (Set BlockIndex))
    -- | Used to cancel the connection if the peer dies off
    , peerKeepAlive :: !(TVar Bool)
    -- | Used to remember to notify the selector if our peer becomes interested
    , peerWatched :: !(TVar Bool)
    -- | The logging handle
    , peerLogger :: !LoggerHandle
    -- | A handle to the information shared with us
    , peerHandle :: !PeerHandle
    -- | A socket we can communicate with the peer on
    , peerSocket :: !Socket
    }

-- | Construct new information that the peer needs
makePeerInfo :: MonadIO m => Socket -> Peer -> LoggerHandle -> PeerHandle -> m PeerInfo
makePeerInfo peerSocket peerPeer peerLogger peerHandle = do
    peerPieces <- newTVarIO Set.empty
    peerRequested <- newTVarIO Nothing
    peerShouldCancel <- newTVarIO Set.empty
    peerKeepAlive <- newTVarIO True
    peerWatched <- newTVarIO False
    return PeerInfo {..}

-- | Represents computations for a peer
newtype PeerM a = PeerM (ReaderT PeerInfo IO a)
        deriving (Functor, Applicative, Monad,
                  MonadReader PeerInfo, MonadIO,
                  MonadThrow)

-- | Ask for a piece of the peer handle
asksHandle :: (PeerHandle -> r) -> PeerM r
asksHandle f = asks (f . peerHandle)

instance HasPieceBuffer PeerM where
    getPieceBuffer = asksHandle handleBuffer

instance HasLogger PeerM where
    getLogger = PeerM (asks peerLogger)

-- | Log something with the source set as this peer
logPeer :: Importance -> [(Text, Text)] -> PeerM ()
logPeer i pairs = do
    peer <- PeerM (asks peerPeer)
    log i ("source" .= peer : pairs)

-- | Get the total number of pieces
getPieceCount :: PeerM Int
getPieceCount = (+1) . snd . bounds <$> asksHandle handlePieces

-- | Run a peer computation given the initial information it needs
runPeerM :: PeerM a -> PeerInfo -> IO a
runPeerM (PeerM rdr) = runReaderT rdr

-- | Increment the upload count of our Status
incrementTrackUp :: Integral a => a -> PeerM ()
incrementTrackUp n = do
    status <- asksHandle handleStatus
    atomically $ modifyTVar' status increment
  where
    increment t@TrackStatus{..} = t { trackUp = trackUp + fromIntegral n}

-- | Increment the download count of our Status
incrementTrackDown :: Integral a => a -> PeerM ()
incrementTrackDown n = do
    status <- asksHandle handleStatus
    atomically $ modifyTVar' status increment
  where
    increment t@TrackStatus{..} = t { trackDown = trackDown + fromIntegral n}

-- | Modify our friendship atomically
modifyFriendship :: (PeerFriendship -> PeerFriendship) -> PeerM ()
modifyFriendship f = do
    friendShip <- asksHandle handleFriendship
    atomically (modifyTVar' friendShip f)

-- | Get some information about our friendship
askFriendship :: (PeerFriendship -> a) -> PeerM a
askFriendship f = f <$> (asksHandle handleFriendship >>= readTVarIO)


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


{- | Recalculate whether or not we're interested

This should be called after either our pieces change,
our their pieces change. This will send the right
message to the peer if our interest changes.
-}
adjustInterest :: PeerM ()
adjustInterest = do
    PeerInfo{..} <- ask
    let PeerHandle{..} = peerHandle
    (interested, shouldInform) <- atomically $ do
        theirPieces <- readTVar peerPieces
        ourPieces   <- readTVar handleOurPieces
        let wanted = Set.difference theirPieces ourPieces
            interested = not (Set.null wanted)
        curr <- readTVar handleFriendship
        writeTVar handleFriendship (curr { peerAmInterested = interested })
        return (interested, peerAmInterested curr /= interested)
    when shouldInform $ do
        let msg = if interested then Interested else UnInterested
        sendMessage msg


-- | Add a piece locally, and increment it's global count.
addPiece :: Int -> PeerM ()
addPiece piece = do
    pieceCounts <- asksHandle handlePieces
    unless (inRange (bounds pieceCounts) piece) cancel
    ourPieces <- asks peerPieces
    atomically $ do
        pieces <- readTVar ourPieces
        unless (Set.member piece pieces) $ do
            let newPieces = Set.insert piece pieces
            newPieces `seq` writeTVar ourPieces newPieces
            modifyTVar' (pieceCounts ! piece) (+ 1)

-- | Send a message to the peer connection
sendMessage :: Message -> PeerM ()
sendMessage msg = do
    socket <- asks peerSocket
    pieceCount <- getPieceCount
    let bytes = encodeMessage pieceCount msg
    incrementTrackUp (BS.length bytes)
    liftIO $ sendAll socket bytes

-- | Send a message to the writer
sendToWriter :: PeerToWriter -> PeerM ()
sendToWriter msg = do
    q <- asksHandle handleToWriter
    atomically $ writeTBQueue q msg


-- | Modify our state based on a message, and send back a reply
reactToMessage :: Message -> PeerM ()
reactToMessage msg = doLog *> case msg of
    KeepAlive -> do
        keepAlive <- asks peerKeepAlive
        atomically $ writeTVar keepAlive True
    Choke     -> modifyFriendship (\f -> f { peerIsChoking = True })
    UnChoke   -> do
        modifyFriendship (\f -> f { peerIsChoking = False })
        requestRarestPiece
    Interested   -> do
        PeerInfo{..} <- ask
        let PeerHandle{..} = peerHandle
        modifyFriendship (\f -> f { peerIsInterested = True })
        whenM (readTVarIO peerWatched) $ do
            atomically $ do
                writeTVar peerWatched False
                writeTBQueue handleToSelector (PeerBecameInterested peerPeer)
    UnInterested -> modifyFriendship (\f -> f { peerIsInterested = False })
    Have piece   -> do
        addPiece piece
        jumpRarestIfFree
    BitField pieces -> do
        forM_ pieces addPiece
        jumpRarestIfFree
    Request info -> do
        me <- asks peerPeer
        let amChoking = askFriendship peerAmChoking
        unlessM amChoking (sendToWriter (PieceRequest me info))
    RecvBlock index bytes -> do
        writeBlockM index bytes
        sendToWriter PieceBufferWritten
        whenJustM (asks peerRequested >>= readTVarIO) request
    Cancel (BlockInfo index _) -> do
        PeerInfo{..} <- ask
        atomically $ modifyTVar' peerShouldCancel (Set.insert index)
    Port _ -> return ()
  where
    jumpRarestIfFree = whenJustM getRarestPiece $ \next -> do
        sendMessage Interested
        requested <- asks peerRequested >>= readTVarIO
        choking   <- askFriendship peerIsChoking
        case (choking, requested) of
            (False, Nothing) -> request next
            (_    , _      ) -> return ()
    doLog = logPeer DebugNoisy ["msg" .= PrettyMessage msg]

-- | React to messages sent by the writer
reactToWriter :: WriterToPeer -> PeerM ()
reactToWriter msg = case msg of
    PieceFulfilled index bytes -> do
        shouldCancel <- asks peerShouldCancel >>= readTVarIO
        unless (Set.member index shouldCancel)
            $ sendMessage (RecvBlock index bytes)
    PieceAcquired piece -> do
        sendMessage (Have piece)
        requested <- asks peerRequested >>= readTVarIO
        choking   <- askFriendship peerIsChoking
        when (not choking && isNothing requested) requestRarestPiece

-- | Loop and react to messages sent by the writer
writerLoop :: PeerM ()
writerLoop = forever $ do
    chan <- asksHandle handleFromWriter
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
    PeerWatchForInterest -> do
        PeerInfo{..} <- ask
        atomically $ writeTVar peerWatched True

-- | A loop handling messages from the manager
selectorLoop :: PeerM ()
selectorLoop = forever $ do
    chan <- asksHandle handleFromSelector
    msg  <- atomically $ readTBQueue chan
    reactToSelector msg


-- | Get the rarest piece that the peer claims to have, and that we don't
getRarestPiece :: PeerM (Maybe Int)
getRarestPiece = do
    theirPieces <- asks peerPieces >>= readTVarIO
    ourPieces   <- asksHandle handleOurPieces >>= readTVarIO
    pieceArr    <- asksHandle handlePieces
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
        PeerInfo{..} <- ask
        atomically $ writeTVar peerRequested (Just piece)
        sendMessage (Request info)
    Nothing -> do
        PeerInfo{..} <- ask
        atomically $ writeTVar peerRequested Nothing

-- | Try and request the rarest piece
requestRarestPiece :: PeerM ()
requestRarestPiece = whenJustM getRarestPiece request


{- | A loop for a process that will kill the peer if messages aren't received

The process reading messages from a socket should set `peerKeepAlive`
to `True` when a message is received, to avoid this process cancelling.
-}
checkKeepAliveLoop :: PeerM ()
checkKeepAliveLoop = forever $ do
    liftIO . threadDelay $ 2 * 60 * 1_000_000
    PeerInfo{..} <- ask
    continue <- atomically $ do
        alive <- readTVar peerKeepAlive
        writeTVar peerKeepAlive False
        return alive
    unless continue cancel

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
    socket <- asks peerSocket
    bytes  <- liftIO $ recv socket 1024
    dlRate <- asksHandle handleDLRate
    now    <- liftIO getCurrentTime
    let byteCount = BS.length bytes
        shiftRates = addDownload byteCount now
    atomically $ modifyTVar' dlRate shiftRates
    incrementTrackDown byteCount
    case parseMessages cb bytes of
        Nothing          -> recvLoop firstParseCallBack
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
    let startAsync = liftIO . async . flip runPeerM r
    checkAlive <- startAsync checkKeepAliveLoop
    stayAlive  <- startAsync sendKeepAliveLoop
    socket     <- startAsync (recvLoop firstParseCallBack)
    selector   <- startAsync selectorLoop
    writer     <- startAsync writerLoop
    (_, e) <- liftIO $ waitAnyCatchCancel [checkAlive, stayAlive, socket, selector, writer]
    logPeer Debug ["peer-cancel" .= e]
