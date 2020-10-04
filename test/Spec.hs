import Test.Hspec ( hspec, describe, it, shouldBe )
import qualified BasedArray as BA

main :: IO ()
main = vectorTest

vectorTest :: IO ()
vectorTest = hspec $ do
  describe "Pointer test" $ do
    it "*VecPointer == *&Vec" $
      let (p,s,o) = BA.vectorPointer (BA.example) in
        BA.recreateVector p s o `shouldBe` BA.example