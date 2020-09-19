import           Data.Char
import           BaseCrypto
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = encoding64Test

ascii :: Gen Char
ascii = elements $ map chr [0..255]

roundTest :: (String -> Either String String) -> (String -> Either String String) -> String -> Expectation
roundTest d e x = getRes (d =<< e x) `shouldBe` x

getRes (Left b)  = "()"
getRes (Right b) = b

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

  describe "manual round test" $ do
    it "Base64" $ do
        roundTest decode64 encode64 "Man"
        roundTest decode64 encode64 "Mann"
        roundTest decode64 encode64 "Mannn"
    it "Base32" $ do
        roundTest decode32 encode32 "Man"
        roundTest decode32 encode32 "Mann"
        roundTest decode32 encode32 "Mannn"
    it "Base16" $ do
        roundTest decode16 encode16 "Man"
        roundTest decode16 encode16 "Mann"
        roundTest decode16 encode16 "Mannn"

  describe "auto one test" $ do
    it "Base64" $ property $
        quickCheck (sample' ascii >>= roundTest decode64 encode64)
    it "Base32" $ property $
        quickCheck (sample' ascii >>= roundTest decode32 encode32)
    it "Base16" $ property $
        quickCheck (sample' ascii >>= roundTest decode16 encode16)

  describe "QuickCheck" $ do
    it "Base64" $ property $ 
      (\x -> getRes (decode64 =<< encode64 x) `shouldBe` (if any (\t -> (ord t) > 255) x then "()" else x))
    it "Base32" $ property $ 
      (\x -> getRes (decode32 =<< encode32 x) `shouldBe` (if any (\t -> (ord t) > 255) x then "()" else x))
    it "Base16" $ property $ 
      (\x -> getRes (decode16 =<< encode16 x) `shouldBe` (if any (\t -> (ord t) > 255) x then "()" else x))
  where
    padd = length . filter (== '=')
