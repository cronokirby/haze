import Relude

import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.HashMap.Strict as HM
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec

import Haze.Bencoding
import Haze.Peer (BlockInfo(..), Message(..), encodeMessage, parseMessage)


main :: IO ()
main = do
    hspec bencodingSpec
    hspec messageSpec
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
            doEncode (BList [BInt 1, BInt 2]) `shouldBe`
                "li1ei2ee"
            doEncode (BList [BString "a", BString "b"])
                `shouldBe` "l1:a1:be"
        it "can encode nested lists" $
            doEncode (BList [BList [BInt 1], BList [BInt 1]]) 
                `shouldBe` "lli1eeli1eee"
        it "can encode hashmaps" $
            doEncode (BMap $ HM.fromList [("A", BInt 1)])
                `shouldBe` "d1:Ai1ee"
        it "encodes hashmaps with sorted keys" $
            doEncode (BMap $ HM.fromList [("B", BInt 2), ("A", BInt 1)])
                `shouldBe` "d1:Ai1e1:Bi2ee"
    describe "Bencoding.decode" $ do
        it "can decode primitive types" $ do
            doDecode "3:foo" `shouldBe` Right (BString "foo")
            doDecode "0:" `shouldBe` Right (BString "")
            doDecode "i42e" `shouldBe` Right (BInt 42)
            doDecode "i-3e" `shouldBe` Right (BInt (-3))
        it "can decode lists" $ do
            doDecode "le" `shouldBe` Right (BList [])
            doDecode "l1:A1:Be" `shouldBe`
                Right (BList [BString "A", BString "B"])
            doDecode "ll1:Aee" `shouldBe` 
                Right (BList [BList [BString "A"]])
        it "can decode maps" $ do
            doDecode "de" `shouldBe` Right (BMap $ HM.fromList [])
            doDecode "d1:K1:Ve" `shouldBe` 
                Right (BMap $ HM.fromList [("K", BString "V")])
  where
    doEncode = encode encodeBen
    doDecode = decode decodeBen

messageSpec :: SpecWith ()
messageSpec =
    describe "Peer.parseMessage" $
        it "can decode basic messages" $ do
            shouldParse "\0\0\0\0" KeepAlive
            shouldParse "\0\0\0\1\0" Choke
            shouldParse "\0\0\0\1\1" UnChoke
            shouldParse "\0\0\0\1\2" Interested
            shouldParse "\0\0\0\1\3" UnInterested
            shouldParse "\0\0\0\5\4\0\0\0\9" (Have 9)
            shouldParse "\0\0\0\13\6\0\0\0\9\0\0\0\9\0\0\0\9" $
                Request (BlockInfo 9 9 9)
            shouldParse "\0\0\0\13\8\0\0\0\9\0\0\0\9\0\0\0\9" $
                Cancel (BlockInfo 9 9 9)
            shouldParse "\0\0\0\3\9\1\0" (Port 256)
            shouldParse "\0\0\0\10\7\0\0\0\9\0\0\0\9A" $
                RecvBlock 9 9 "A"
  where
    shouldParse bs res = 
        parseOnly parseMessage bs
        `shouldBe` Right res


propertyTests :: IO ()
propertyTests = void $
    checkParallel $ Group "Bencoding Properties" 
        [ ("prop_bencoding", propBencoding)
        , ("prop_message", propMessage)
        ]

propBencoding :: Property
propBencoding =
    property $ do
        ben <- forAll genBencoding
        decode decodeBen (encode encodeBen ben) === Right ben
  where
    genBencoding :: MonadGen m => m Bencoding
    genBencoding = Gen.recursive Gen.choice
        [genBInt, genBString]
        [genBList, genBMap]
    genBInt :: MonadGen m => m Bencoding
    genBInt = BInt <$> Gen.int64 (Range.linear (-100) 100)
    genBString :: MonadGen m => m Bencoding
    genBString = BString <$> Gen.bytes (Range.linear 0 100)
    genBList :: MonadGen m => m Bencoding
    genBList = BList <$> Gen.list (Range.linear 0 32) genBencoding
    genBMap :: MonadGen m => m Bencoding 
    genBMap = BMap . HM.fromList <$> Gen.list (Range.linear 0 32) pair
      where
        pair = (,) <$> Gen.bytes (Range.linear 0 10) <*> genBencoding


propMessage :: Property
propMessage =
    property $ do
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
        , liftA3 RecvBlock genInt genInt genBS
        ]
    genInt :: MonadGen m => m Int
    genInt = Gen.int (Range.linear 0 100)
    genBS :: MonadGen m => m ByteString
    genBS = Gen.bytes (Range.linear 0 32)
    genBlockInfo :: MonadGen m => m BlockInfo
    genBlockInfo = liftA3 BlockInfo genInt genInt genInt
