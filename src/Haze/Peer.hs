{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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

import           Control.Concurrent.STM.TBQueue ( TBQueue
                                                , writeTBQueue
                                                )
import           Control.Exception.Safe         ( Exception
                                                , MonadThrow
                                                , throw
                                                )
import           Data.Attoparsec.ByteString    as AP
import           Data.Array                     ( Array
                                                , (!)
                                                , bounds
                                                )
import qualified Data.ByteString               as BS
import           Data.Ix                        ( inRange )
import qualified Data.Set                      as Set
import           Network.Socket                 ( PortNumber )

import           Haze.Bits                      ( encodeIntegralN
                                                , parseInt
                                                , parse16
                                                )
import           Haze.Messaging                 ( PeerToWriter(..)
                                                )
import           Haze.PieceBuffer               ( PieceBuffer
                                                , BlockInfo(..)
                                                , BlockIndex(..)
                                                , makeBlockInfo
                                                , HasPieceBuffer(..)
                                                , writeBlockM
                                                )


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
    { peerIsChoking :: !Bool -- ^ We're being choked by the peer
    -- | The peer is interested in us
    , peerIsInterested :: !Bool
    -- | We are choking the peer
    , peerAmChoking :: !Bool
    -- | We're interested in the peer 
    , peerAmInterested :: !Bool
    -- | The set of pieces this peer has
    , peerPieces :: !(Set Int)
    }

-- | The peer state at the start of communication
initialPeerState :: PeerState
initialPeerState = PeerState True False True False Set.empty


-- | The information needed in a peer computation
data PeerMInfo = PeerMInfo
    { peerMState :: !(IORef PeerState) -- ^ The local state
    -- | A map from piece index to piece count, used for rarity calcs
    , peerMPieces :: !(Array Int (TVar Int))
    -- | The piece buffer shared with everyone else
    , peerMBuffer :: !(TVar PieceBuffer)
    -- | The out bound message queue to the piece writer
    , peerMToWriter :: !(TBQueue PeerToWriter)
    }

-- | Represents computations for a peer
newtype PeerM a = PeerM (ReaderT PeerMInfo IO a)
        deriving (Functor, Applicative, Monad,
                  MonadReader PeerMInfo, MonadIO, MonadThrow)

instance MonadState PeerState PeerM where
    state f = do
        stRef <- asks peerMState
        st    <- readIORef stRef
        let (a, st') = f st
        writeIORef stRef st'
        return a

instance HasPieceBuffer PeerM where
    getPieceBuffer = asks peerMBuffer


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
    pieces <- asks peerMPieces
    unless (inRange (bounds pieces) piece) cancel
    atomically $ modifyTVar' (pieces ! piece) (+ 1)
    modify (addLocalPiece piece)
  where
    addLocalPiece piece ps =
        let pieces = peerPieces ps
        in  ps { peerPieces = Set.insert piece pieces }

-- | Send a message to the writer
sendToWriter :: PeerToWriter -> PeerM ()
sendToWriter msg = do
    q <- asks peerMToWriter
    atomically $ writeTBQueue q msg


-- | Modify our state based on a message, and send back a reply
reactToMessage :: Message -> PeerM ()
reactToMessage msg = case msg of
    Choke                 -> modify (\ps -> ps { peerIsChoking = True })
    UnChoke               -> modify (\ps -> ps { peerIsChoking = False })
    Interested            -> modify (\ps -> ps { peerIsInterested = True })
    UnInterested          -> modify (\ps -> ps { peerIsInterested = False })
    Have piece            -> addPiece piece
    Request info          -> sendToWriter (PieceRequest info)
    RecvBlock index bytes -> writeBlockM index bytes
    _                     -> undefined
