import           Data.Char
import           Lib
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = encoding64Test

ascii :: Gen Char
ascii = elements $ map chr [0..255]

encoding64Test :: IO ()
encoding64Test = hspec $ do
  describe "correct paddings" $ do
    it "Base64" $ do
      padd (getRes $ encode64 "Man") `shouldBe` (0 :: Int)
      padd (getRes $ encode64 "Mann") `shouldBe` (2 :: Int)
      padd (getRes $ encode64 "Mannn") `shouldBe` (1 :: Int)
    it "Base32" $ do
      padd (getRes $ encode32 "Man") `shouldBe` (3 :: Int)
      padd (getRes $ encode32 "Mann") `shouldBe` (1 :: Int)
      padd (getRes $ encode32 "Mannn") `shouldBe` (0 :: Int)
    it "Base16" $ do
      padd (getRes $ encode16 "Man") `shouldBe` (0 :: Int)
      padd (getRes $ encode16 "Man") `shouldBe` (0 :: Int)
      padd (getRes $ encode16 "Man") `shouldBe` (0 :: Int)

  describe "round test" $ do
    it "Base64" $ property $
      quickCheck (sample' ascii >>= (\x -> getRes (decode64 =<< encode64 x) `shouldBe` x))
    it "Base32" $ property $
        quickCheck (sample' ascii >>= (\x -> getRes (decode32 =<< encode32 x) `shouldBe` x))
    it "Base16" $ property $
        quickCheck (sample' ascii >>= (\x -> getRes (decode16 =<< encode16 x) `shouldBe` x))
  where
    getRes (Left b)  = b
    getRes (Right b) = b
    padd = length . filter (== '=')
