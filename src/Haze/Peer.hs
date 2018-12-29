{- |
Description: Contains types and functions related to peer communication.
-}
module Haze.Peer
    ( BlockInfo(..)
    , Message(..)
    , encodeMessage
    , parseMessage
    )
where

import           Relude

import           Data.Attoparsec.ByteString    as AP
import qualified Data.ByteString               as BS
import           Network.Socket                 ( PortNumber )

import           Haze.Bits                      ( encodeIntegralN
                                                , parseInt
                                                , parse16
                                                )


{- | Represents the information related to a block we can request

Contains index, offset, and block length.
-}
data BlockInfo = BlockInfo Int Int Int deriving (Eq, Show)

instance Hashable BlockInfo where
    hashWithSalt i (BlockInfo a b c) = hashWithSalt i [a, b, c]


-- | The messages sent between peers in a torrent
data Message
    -- | Used to keep the connection open (think PING)
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
    | RecvBlock !Int !Int !ByteString
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
    RecvBlock a b block ->
        let len = BS.length block
        in  BS.pack (encInt (len + 9) ++ [7] ++ encInt a ++ encInt b) <> block
    Cancel block -> BS.pack $ encInt 13 ++ [8] ++ encBlock block
    Port p -> BS.pack $ encInt 3 ++ [9] ++ drop 2 (encInt (fromIntegral p))
  where
    encInt :: Int -> [Word8]
    encInt = encodeIntegralN 4
    encBlock :: BlockInfo -> [Word8]
    encBlock (BlockInfo a b c) = foldMap encInt [a, b, c]


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
    parseID 13 6 = Request <$> liftA3 BlockInfo parseInt parseInt parseInt
    parseID 13 8 = Cancel <$> liftA3 BlockInfo parseInt parseInt parseInt
    parseID 3  9 = Port <$> parse16
    parseID ln 7 = do
        index <- parseInt
        begin <- parseInt
        let blockLen = ln - 9
        RecvBlock index begin <$> AP.take blockLen
    parseID _ _ = fail "Unrecognised ID, or bad length"
