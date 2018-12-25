import Control.Monad.Reader

data Config = Config {discountRate :: Float, currencySym :: String}
appCfg = (Config 10 "R")

discount :: Float -> Reader Config Float
discount amt = do
  discountRate' <- asks discountRate
  return (amt * (1 - discountRate' / 100))

main = do
  print $ runReader (discount 100) appCfg

  let doDoubleDiscount = (discount 100 >>= discount >>= display)
  putStrLn $ runReader doDoubleDiscount appCfg
      

display :: Float -> Reader Config String
display amt = do
  currencySym' <- asks currencySym
  return (currencySym' ++ " " ++ (show amt))
