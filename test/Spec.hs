import Test.Hspec
import Data.Either(fromRight)
import Lib

main :: IO ()
main = encoding64Test

encoding64Test :: IO ()
encoding64Test = hspec $ do
    it "correct paddings" $ do
        (length . filter (== '=') $ f "Man") `shouldBe` (0 :: Int)
        (length . filter (== '=') $ f "Mann") `shouldBe` (2 :: Int)
        (length . filter (== '=') $ f "Mannn") `shouldBe` (1 :: Int)
    it "correct results" $ 
        f "FNASDISFNOIAFIOASFNOIAFNIAGFNAGOANGDIGNWAIGNGNSOGIASNGISAGANISGOASDIGNASG" `shouldBe` "Rk5BU0RJU0ZOT0lBRklPQVNGTk9JQUZOSUFHRk5BR09BTkdESUdOV0FJR05HTlNPR0lBU05HSVNBR0FOSVNHT0FTRElHTkFTRw==" --lmao test

        -- TODO: QuickTest random values instead of this crap
    where 
        f = fromRight "" . encode64
        g = fromRight "" . decode64