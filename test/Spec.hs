import           Relude

import           Data.Array                     ( listArray )
import           Data.Attoparsec.ByteString     ( parseOnly )
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe                     ( fromJust )
import qualified Data.Set                      as Set
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import qualified Path
import           Test.Hspec

import           Haze.Bencoding
import           Haze.Peer                      ( Message(..)
                                                , encodeMessage
                                                , parseMessage
                                                , parseMessages
                                                , firstParseCallBack
                                                )
import           Haze.PieceBuffer               ( BlockIndex(..)
                                                , BlockInfo
                                                , makeBlockInfo
                                                )
import           Haze.PieceWriter               ( FileStructure(..)
                                                , SplitPiece(..)
                                                , makeFileStructure
                                                , PieceMapping(..)
                                                , PieceLocation(..)
                                                , CompleteLocation(..)
                                                , EmbeddedLocation(..)
                                                , makeMapping
                                                )
import           Haze.Tracker                   ( SHAPieces(..)
                                                , FileInfo(..)
                                                , FileItem(..)
                                                )


main :: IO ()
main = do
    hspec bencodingSpec
    hspec messageSpec
    hspec pieceWriterSpec
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

pieceWriterSpec :: SpecWith ()
pieceWriterSpec = do
    makeMappingSpec
    makeFileStructureSpec


makeMappingSpec :: SpecWith ()
makeMappingSpec = describe "PieceWriter.makeMapping" $ do
    it "works for single files" $ do
        let m1 = [[("foo.txt", 0, 2, "piece-0.bin")]]
        singleFile "foo.txt" 2 `mappingShouldBe` m1
        let
            m2 =
                [ [("foo.txt", 0, 2, "piece-0.bin")]
                , [("foo.txt", 2, 1, "piece-1.bin")]
                ]
        singleFile "foo.txt" 3 `mappingShouldBe` m2
    it "works for multiple files with even division" $ do
        let m1 =
                [ [("/rel/foo.txt", 0, 2, "/rel/piece-0.bin")]
                , [("/rel/bar.txt", 0, 2, "/rel/piece-1.bin")]
                ]
        multiFiles [("foo.txt", 2), ("bar.txt", 2)] `mappingShouldBe` m1
        let m2 =
                [ [("/rel/foo.txt", 0, 2, "/rel/piece-0.bin")]
                , [("/rel/bar.txt", 0, 2, "/rel/piece-1.bin")]
                , [("/rel/bar.txt", 2, 1, "/rel/piece-2.bin")]
                ]
        multiFiles [("foo.txt", 2), ("bar.txt", 3)] `mappingShouldBe` m2
    it "works for multiple files with uneven division" $ do
        let m1 =
                [ [("/rel/foo.txt", 0, 2, "/rel/piece-0.bin")]
                , [ ("/rel/foo.txt", 2, 1, "/rel/foo.txt.end")
                  , ("/rel/bar.txt", 0, 1, "/rel/bar.txt.start")
                  ]
                , [("/rel/bar.txt", 1, 1, "/rel/piece-2.bin")]
                ]
        multiFiles [("foo.txt", 3), ("bar.txt", 2)] `mappingShouldBe` m1
    it "works for very small files" $ do
        let f1 = [("foo.txt", 3), ("bar.txt", 1), ("baz.txt", 2)]
            m1 =
                [ [("/rel/foo.txt", 0, 2, "/rel/piece-0.bin")]
                , [ ("/rel/foo.txt", 2, 1, "/rel/foo.txt.end")
                  , ("/rel/bar.txt", 0, 1, "/rel/bar.txt.start")
                  ]
                , [("/rel/baz.txt", 0, 2, "/rel/piece-2.bin")]
                ]
        multiFiles f1 `mappingShouldBe` m1
  where
    makeAbsFile = fromJust . Path.parseAbsFile . ("/" ++)
    root        = fromJust (Path.parseAbsDir "/")
    relRoot     = fromJust (Path.parseRelDir "./rel")
    makeFileItem path size =
        FileItem (fromJust (Path.parseRelFile path)) size Nothing
    singleFile path size = SingleFile (makeFileItem path size)
    multiFiles  = MultiFile relRoot . map (uncurry makeFileItem)
    smallPieces = SHAPieces 2 ""
    mappingShouldBe info locations =
        let makeLoc (efs, o, i, cfs) =
                    let complete = CompleteLocation (makeAbsFile cfs)
                        embedded = EmbeddedLocation (makeAbsFile efs) o i
                    in  PieceLocation embedded complete
            locs        = map makeLoc <$> locations
            mapping     = listArray (0, length locs - 1) locs
            madeMapping = makeMapping info smallPieces root
        in  madeMapping `shouldBe` PieceMapping mapping

makeFileStructureSpec :: SpecWith ()
makeFileStructureSpec = describe "PieceWriter.makeFileStructure" $ do
    it "works for single files with even division" $ do
        let splits = [makeNormal "piece-0.bin"]
            deps   = [("foo.txt", ["piece-0.bin"])]
        singleFile "foo.txt" 2 `fsShouldBe` makeStructure splits deps
    it "works for single files with uneven division" $ do
        let splits = [makeNormal "piece-0.bin", makeNormal "piece-1.bin"]
            deps   = [("foo.txt", ["piece-0.bin", "piece-1.bin"])]
        singleFile "foo.txt" 3 `fsShouldBe` makeStructure splits deps
    it "works for multiple files with even division" $ do
        let fs = [("foo.txt", 2), ("bar.txt", 2)]
            splits =
                [makeNormal "/rel/piece-0.bin", makeNormal "/rel/piece-1.bin"]
            deps =
                [ ("/rel/foo.txt", ["/rel/piece-0.bin"])
                , ("/rel/bar.txt", ["/rel/piece-1.bin"])
                ]
        multiFiles fs `fsShouldBe` makeStructure splits deps
    it "works for multiple files with uneven division" $ do
        let fs = [("foo.txt", 3), ("bar.txt", 2)]
            splits =
                [ makeNormal "/rel/piece-0.bin"
                , makeSplits
                    [(1, "/rel/foo.txt.end"), (1, "/rel/bar.txt.start")]
                , makeNormal "/rel/piece-2.bin"
                ]
            deps =
                [ ("/rel/foo.txt", ["/rel/piece-0.bin", "/rel/foo.txt.end"])
                , ("/rel/bar.txt", ["/rel/bar.txt.start", "/rel/piece-2.bin"])
                ]
        multiFiles fs `fsShouldBe` makeStructure splits deps
    it "works for small files" $ do
        let fs = [("foo.txt", 3), ("bar.txt", 1), ("baz.txt", 2)]
            splits =
                [ makeNormal "/rel/piece-0.bin"
                , makeSplits [(1, "rel/foo.txt.end"), (1, "/rel/bar.txt.start")]
                , makeNormal "/rel/piece-2.bin"
                ]
            deps =
                [ ("/rel/foo.txt", ["/rel/piece-0.bin", "/rel/foo.txt.end"])
                , ("/rel/bar.txt", ["/rel/bar.txt.start"])
                , ("/rel/baz.txt", ["/rel/piece-2.bin"])
                ]
        multiFiles fs `fsShouldBe` makeStructure splits deps
  where
    makeAbsFile = fromJust . Path.parseAbsFile . ("/" ++)
    root        = fromJust (Path.parseAbsDir "/")
    relRoot     = fromJust (Path.parseRelDir "./rel")
    makeFileItem path size =
        FileItem (fromJust (Path.parseRelFile path)) size Nothing
    singleFile path size = SingleFile (makeFileItem path size)
    multiFiles  = MultiFile relRoot . map (uncurry makeFileItem)
    smallPieces = SHAPieces 2 ""
    makeNormal  = NormalPiece . makeAbsFile
    makeSplits  = SplitPieces . map (\(i, f) -> (i, makeAbsFile f))
    makeStructure splits deps =
        let splitArr = listArray (0, length splits - 1) splits
            absDeps =
                    map (\(f, fs) -> (makeAbsFile f, map makeAbsFile fs)) deps
        in  FileStructure splitArr absDeps
    fsShouldBe info res =
        let mapping = makeMapping info smallPieces root
        in  makeFileStructure mapping `shouldBe` res


propertyTests :: IO ()
propertyTests = void $ checkParallel $ Group
    "Bencoding Properties"
    [ ("prop_bencoding"    , propBencoding)
    , ("prop_message"      , propMessage)
    , ("prop_multi_message", propMultiMessage)
    ]

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
    parseOnly parseMessage (encodeMessage 16 msg) === Right msg

propMultiMessage :: Property
propMultiMessage = property $ do
    msgs <- forAll genMessages
    doParse (encodeMessages msgs) === Just msgs
  where
    encodeMessages :: [Message] -> ByteString
    encodeMessages = foldMap (encodeMessage 16)
    doParse :: ByteString -> Maybe [Message]
    doParse = fmap fst . parseMessages firstParseCallBack
    genMessages :: MonadGen m => m [Message]
    genMessages = Gen.list (Range.linear 0 20) genMessage

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
    , BitField . Set.fromList <$> Gen.list (Range.linear 0 32) genPiece
    ]
genPiece :: MonadGen m => m Int
genPiece = Gen.int (Range.linear 0 15)
genInt :: MonadGen m => m Int
genInt = Gen.int (Range.linear 0 100)
genBS :: MonadGen m => m ByteString
genBS = Gen.bytes (Range.linear 0 32)
genBlockIndex :: MonadGen m => m BlockIndex
genBlockIndex = liftA2 BlockIndex genInt genInt
genBlockInfo :: MonadGen m => m BlockInfo
genBlockInfo = liftA3 makeBlockInfo genInt genInt genInt
