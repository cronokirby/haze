import           Relude

import           Data.Attoparsec.ByteString     ( parseOnly )
import qualified Data.ByteString               as BS
import qualified Data.HashMap.Strict           as HM
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Hspec

import           Haze.Bencoding
import           Haze.Peer                      ( Message(..)
                                                , encodeMessage
                                                , parseMessage
                                                )
import           Haze.PieceBuffer               ( BlockIndex(..)
                                                , BlockInfo
                                                , makeBlockInfo
                                                , blockInfoMatches
                                                , sizedPieceBuffer
                                                , nextBlock
                                                , writeBlock
                                                , saveCompletePieces
                                                , bufferBytes
                                                )
import           Haze.Tracker                   ( SHAPieces(..) )


main :: IO ()
main = do
    hspec bencodingSpec
    hspec messageSpec
    hspec pieceBufferSpec
    propertyTests


bencodingSpec :: SpecWith ()
bencodingSpec = do
    describe "Bencoding.encode" $ do
        it "can encode the primitive types" $ do
            doEncode (BString "foo") `shouldBe` "3:foo"
            doEncode (BString "") `shouldBe` "0:"
            doEncode (BInt 42) `shouldBe` "i42e"
            doEncode (BInt (-3)) `shouldBe` "i-3e"
        it "can encode lists" $ do
            doEncode (BList [BInt 1, BInt 2]) `shouldBe` "li1ei2ee"
            doEncode (BList [BString "a", BString "b"]) `shouldBe` "l1:a1:be"
        it "can encode nested lists" $ do
            doEncode (BList [BList []]) `shouldBe` "llee"
            doEncode (BList [BList [BInt 1], BList [BInt 1]])
                `shouldBe` "lli1eeli1eee"
        it "can encode hashmaps" $ do
            doEncode (BMap HM.empty) `shouldBe` "de"
            doEncode (BMap $ HM.fromList [("A", BInt 1)]) `shouldBe` "d1:Ai1ee"
        it "encodes hashmaps with sorted keys"
            $ doEncode (BMap $ HM.fromList [("B", BInt 2), ("A", BInt 1)])
            `shouldBe` "d1:Ai1e1:Bi2ee"
    describe "Bencoding.decode" $ do
        it "can decode primitive types" $ do
            doDecode "3:foo" `shouldBe` Right (BString "foo")
            doDecode "0:" `shouldBe` Right (BString "")
            doDecode "i42e" `shouldBe` Right (BInt 42)
            doDecode "i-3e" `shouldBe` Right (BInt (-3))
        it "can decode lists" $ do
            doDecode "le" `shouldBe` Right (BList [])
            doDecode "l1:A1:Be"
                `shouldBe` Right (BList [BString "A", BString "B"])
            doDecode "ll1:Aee" `shouldBe` Right (BList [BList [BString "A"]])
        it "can decode maps" $ do
            doDecode "de" `shouldBe` Right (BMap $ HM.fromList [])
            doDecode "d1:K1:Ve"
                `shouldBe` Right (BMap $ HM.fromList [("K", BString "V")])
  where
    doEncode = encode encodeBen
    doDecode = decode decodeBen

messageSpec :: SpecWith ()
messageSpec =
    describe "Peer.parseMessage" $ it "can decode basic messages" $ do
        shouldParse "\0\0\0\0"           KeepAlive
        shouldParse "\0\0\0\1\0"         Choke
        shouldParse "\0\0\0\1\1"         UnChoke
        shouldParse "\0\0\0\1\2"         Interested
        shouldParse "\0\0\0\1\3"         UnInterested
        shouldParse "\0\0\0\5\4\0\0\0\9" (Have 9)
        shouldParse "\0\0\0\13\6\0\0\0\9\0\0\0\9\0\0\0\9"
            $ Request (makeBlockInfo 9 9 9)
        shouldParse "\0\0\0\13\8\0\0\0\9\0\0\0\9\0\0\0\9"
            $ Cancel (makeBlockInfo 9 9 9)
        shouldParse "\0\0\0\3\9\1\0" (Port 256)
        shouldParse "\0\0\0\10\7\0\0\0\9\0\0\0\9A"
            $ RecvBlock (BlockIndex 9 9) "A"
    where shouldParse bs res = parseOnly parseMessage bs `shouldBe` Right res


pieceBufferSpec :: SpecWith ()
pieceBufferSpec = do
    describe "PieceBuffer.blockInfoMatches" $ do
        let info = makeBlockInfo 0 0 4
            shouldMatch x bs = blockInfoMatches info x bs `shouldBe` True
            shouldNotMatch x bs = blockInfoMatches info x bs `shouldBe` False
        it "returns True when everything matches" $ do
            shouldMatch (BlockIndex 0 0) "1234"
            shouldMatch (BlockIndex 0 0) "____"
        it "returns False when the index doesn't match" $ do
            shouldNotMatch (BlockIndex 1 0) "1234"
            shouldNotMatch (BlockIndex 9 0) "____"
        it "returns False when the offset doesn't match" $ do
            shouldNotMatch (BlockIndex 0 1) "____"
            shouldNotMatch (BlockIndex 0 9) "1234"
        it "returns False when the byte length doesn't match" $ do
            shouldNotMatch (BlockIndex 0 0) ""
            shouldNotMatch (BlockIndex 0 0) "TOO LONG"
    describe "PieceBuffer.nextBlock" $ do
        it "fetches the first available block"
            $ nextBlockShouldBe 0 (Just (makeBlockInfo 0 0 1))
        it "returns Nothing for invalid indices" $ do
            nextBlockShouldBe 100  Nothing
            nextBlockShouldBe (-1) Nothing
            nextBlockShouldBe 2    Nothing
        it "returns Nothing when fetching a full piece"
            $ let (_, buffer') = nextBlock 0 oneBuffer
              in  fst (nextBlock 0 buffer') `shouldBe` Nothing
    describe "PieceBuffer.bufferBytes" $ do
        it "returns a bytestring with the same length as the buffer" $ do
            BS.length (bufferBytes oneBuffer) `shouldBe` 1
            BS.length (bufferBytes awkwardBuffer) `shouldBe` 3
            BS.length (bufferBytes bigBuffer) `shouldBe` 4
        it "returns just underscores for an empty buffer" $ do
            oneBuffer `bytesShouldBe` "_"
            bigBuffer `bytesShouldBe` "____"
    describe "PieceBuffer.writeBlock" $ do
        it "lets up fill up a piece" $ do
            bigBuffer1 `bytesShouldBe` "____"
            bigBuffer2 `bytesShouldBe` "12__"
        it "does nothing if the piece is already written" $ do
            writeBlock (BlockIndex 0 0) "X" bigBuffer2 `bytesShouldBe` "12__"
            writeBlock (BlockIndex 0 1) "X" bigBuffer2 `bytesShouldBe` "12__"
        it "does not change the length of a piece" $ do
            let awkward1 = writeBlock (BlockIndex 0 0) "12" awkwardBuffer
                awkward2 = writeBlock (BlockIndex 0 2) "34" awkward1
            awkward2 `bytesShouldBe` "123"
        it "can handle the final piece of a buffer" $ do
            let buffer  = sizedPieceBuffer 3 (SHAPieces 2 "") 2
                written = writeBlock (BlockIndex 1 0) "12" buffer
            written `bytesShouldBe` "__1"
    describe "PieceBuffer.saveCompletePieces" $ do
        let (pieces1, buf1) = saveCompletePieces bigBuffer2
            (pieces2, buf2) = saveCompletePieces bigBuffer1
        it "will replace the pieces by saved bytes" $ do
            buf1 `bytesShouldBe` "ss__"
            buf2 `bytesShouldBe` "____"
        it "will return just the pieces that are complete" $ do
            pieces1 `shouldBe` [(0, "12")]
            pieces2 `shouldBe` []
  where
    nextBlockShouldBe piece target =
        fst (nextBlock piece oneBuffer) `shouldBe` target
    bytesShouldBe buf target = bufferBytes buf `shouldBe` target
    oneBuffer     = sizedPieceBuffer 1 (SHAPieces 1 "") 1
    awkwardBuffer = sizedPieceBuffer 3 (SHAPieces 3 "") 2
    bigBuffer     = sizedPieceBuffer 4 (SHAPieces 2 "") 1
    bigBuffer1    = writeBlock (BlockIndex 0 0) "1" bigBuffer
    bigBuffer2    = writeBlock (BlockIndex 0 1) "2" bigBuffer1


propertyTests :: IO ()
propertyTests = void $ checkParallel $ Group
    "Bencoding Properties"
    [("prop_bencoding", propBencoding), ("prop_message", propMessage)]

propBencoding :: Property
propBencoding = property $ do
    ben <- forAll genBencoding
    decode decodeBen (encode encodeBen ben) === Right ben
  where
    genBencoding :: MonadGen m => m Bencoding
    genBencoding =
        Gen.recursive Gen.choice [genBInt, genBString] [genBList, genBMap]
    genBInt :: MonadGen m => m Bencoding
    genBInt = BInt <$> Gen.int64 (Range.linear (-100) 100)
    genBString :: MonadGen m => m Bencoding
    genBString = BString <$> Gen.bytes (Range.linear 0 100)
    genBList :: MonadGen m => m Bencoding
    genBList = BList <$> Gen.list (Range.linear 0 32) genBencoding
    genBMap :: MonadGen m => m Bencoding
    genBMap = BMap . HM.fromList <$> Gen.list (Range.linear 0 32) pair
        where pair = (,) <$> Gen.bytes (Range.linear 0 10) <*> genBencoding


propMessage :: Property
propMessage = property $ do
    msg <- forAll genMessage
    parseOnly parseMessage (encodeMessage msg) === Right msg
  where
    genMessage :: MonadGen m => m Message
    genMessage = Gen.choice
        [ return KeepAlive
        , return Choke
        , return UnChoke
        , return Interested
        , return UnInterested
        , Have <$> genInt
        , Port <$> Gen.integral_ (Range.linear 0 100)
        , Request <$> genBlockInfo
        , Cancel <$> genBlockInfo
        , liftA2 RecvBlock genBlockIndex genBS
        ]
    genInt :: MonadGen m => m Int
    genInt = Gen.int (Range.linear 0 100)
    genBS :: MonadGen m => m ByteString
    genBS = Gen.bytes (Range.linear 0 32)
    genBlockIndex :: MonadGen m => m BlockIndex
    genBlockIndex = liftA2 BlockIndex genInt genInt
    genBlockInfo :: MonadGen m => m BlockInfo
    genBlockInfo = liftA3 makeBlockInfo genInt genInt genInt
