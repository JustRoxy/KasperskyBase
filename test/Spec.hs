import           Test.Hspec
import qualified BasedArray as BA

main :: IO ()
main = encoding64Test

encoding64Test :: IO ()
encoding64Test = hspec $ do
  describe "Pointer test" $ do
    it "*VecPointer == *&Vec" $
      let (p,s,o) = BA.vectorPointer (BA.example) in
        BA.recreateVector p s o `shouldBe` BA.example