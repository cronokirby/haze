{- |
Description: Contains types and functions related to peer communication.
-}
module Haze.Peer
    ( Block(..)
    , ExpectedBlocks
    , Message(..)
    , parseMessage
    )
where

import Relude

import Data.Attoparsec.ByteString as AP
import Data.Bits ((.|.), shift)
import qualified Data.HashSet as HS
import Network.Socket (PortNumber)


-- | Represents the information related to a block we can request
data Block = Block
    { blockIndex :: Int -- ^ The 0 based piece index of this block
    , blockOffset :: Int -- ^ The 0 based bytes offset in the piece
    , blockSize :: Int -- ^ The size of this block (bytes)
    }
    deriving (Eq)

instance Hashable Block where
    hashWithSalt i (Block a b c) = hashWithSalt i [a, b, c]


-- | Used to keep track of what blocks we expect to receive
type ExpectedBlocks = HS.HashSet Block



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
    | Have Int
    {- | The sender is requesting a block in a certain piece.

    The first Int represents which piece is being asked for. The 2nd
    the zero base byte offset within the piece. And the 3rd the length.
    -}
    | Request Int Int Int
    {- | The sender is fulfilling a block request.

    The first 2 bytes represent the piece index and byte offset.
    The final part is the actual bytes constituting the block.
    -}
    | RecvBlock Int Int ByteString
    -- | The send is no longer requesting a block
    | Cancel Int Int Int
    -- | The port this peer's DHT is listening on
    | Port PortNumber
    deriving (Eq, Show)



{- | Parse a message encoded as as string of bytes

Takes the current size of the block we're expected to receive, if any.
If no block is expected, but a Receive message is parsed, then this
parser will fail.
-}
parseMessage :: ExpectedBlocks -> AP.Parser Message
parseMessage blocks = do
    len <- parseInt
    if len == 0
        then return KeepAlive
        else AP.anyWord8 >>= parseID blocks len
  where
    parseID :: ExpectedBlocks -> Int -> Word8 -> AP.Parser Message
    parseID _ 1  0 = return Choke
    parseID _ 1  1 = return UnChoke
    parseID _ 1  2 = return Interested 
    parseID _ 1  3 = return UnInterested
    parseID _ 5  4 = Have <$> parseInt
    parseID _ 13 6 = Request <$> parseInt <*> parseInt <*> parseInt
    parseID _ 13 8 = Cancel <$> parseInt <*> parseInt <*> parseInt
    parseID _ 3  9 = Port <$> parsePort
    parseID blocks' len 7 = do
        index <- parseInt
        begin <- parseInt
        let blockLen = len - 9
            block = Block index begin blockLen
        if HS.member block blocks'
            then RecvBlock index begin <$> AP.take blockLen
            else fail "Unrecognised block received"
    parseID _ _  _ = fail "Unrecognised ID, or bad length"

parseInt :: AP.Parser Int
parseInt = do
    b1 <- AP.anyWord8
    b2 <- AP.anyWord8 
    b3 <- AP.anyWord8
    b4 <- AP.anyWord8
    return (makeWord32 [b4, b3, b2, b1])
  where
    makeWord32 :: [Word8] -> Int
    makeWord32 = foldr (\b acc -> shift acc 8 .|. fromIntegral b) 0

parsePort :: AP.Parser PortNumber
parsePort = do
    a <- AP.anyWord8
    b <- AP.anyWord8
    return . fromInteger . makeWord16 $ [b, a]
  where
    makeWord16 :: [Word8] -> Integer
    makeWord16 = foldr (\b acc -> shift acc 8 .|. fromIntegral b) 0
