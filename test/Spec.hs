import Relude

import qualified Data.HashMap.Strict as HM
import Test.Hspec

import Haze.Bencoding


main :: IO ()
main = hspec bencodingSpec


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
