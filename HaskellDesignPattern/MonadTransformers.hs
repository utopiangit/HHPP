import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Writer

data Config = Config {discountRate :: Float, currencySym :: String}
appCfg = (Config 10 "R")

discount :: Float -> Reader Config Float
discount amt = do
  discountRate' <- asks discountRate
  return (amt * (1 - discountRate' / 100))

main = do
  print $ runReader (discount 100) appCfg
  putStrLn $ runReader doDoubleDiscount appCfg
  print $ runWriter (runReaderT doDoubleDiscountWR appCfg)
    where 
      doDoubleDiscount = (discount 100 >>= discount >>= display)
      doDoubleDiscountWR = (discountWR 100 >>= discountWR >>= displayWR)
      

display :: Float -> Reader Config String
display amt = do
  currencySym' <- asks currencySym
  return (currencySym' ++ " " ++ (show amt))

discountWR :: Float -> ReaderT Config (Writer String) Float
discountWR amt = do
  discountRate' <- asks discountRate
  let discounted = amt * (1 - discountRate' / 100)
  tell $ "-Discount " ++ (show amt) ++ " = " ++ (show discounted)
  return discounted

displayWR :: Float -> ReaderT Config (Writer String) String
displayWR amt = do
  currencySym' <- asks currencySym
  tell " > Displaying..."
  return (currencySym' ++ " " ++ (show amt))