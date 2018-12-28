{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Writer

data Config = Config {discountRate :: Float, currencySym :: String}
appCfg = (Config 10 "R")

type App = ReaderT Config (Writer String)

main = do
  print $ doApp doDoubleDiscount
  print <$> doAppIO doDoubleDiscountIO
    where
      doDoubleDiscount = (discountWR 100 >>= discountWR >>= displayWR)
      doDoubleDiscountIO = (discountWRIO 100 >>= discountWRIO >>= displayWRIO)

doApp :: App a -> (a, String)
doApp app = runWriter (runReaderT app appCfg)

discountWR :: Float -> App Float
discountWR amt = do
  discountRate' <- asks discountRate
  let discounted = amt * (1 - discountRate' / 100)
  tell $ "-Discount " ++ (show amt) ++ " = " ++ (show discounted)
  return discounted

displayWR :: Float -> App String
displayWR amt = do
  currencySym' <- asks currencySym
  tell " > Displaying..."
  return (currencySym' ++ " " ++ (show amt))

newtype AppIO a = AppIO {runAppIO :: ReaderT Config (WriterT String IO) a}
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadWriter String, MonadIO)

doAppIO :: AppIO a -> IO (a, String)
doAppIO app = runWriterT (runReaderT (runAppIO app) appCfg)

discountWRIO :: Float -> AppIO Float
discountWRIO amt = do
  liftIO $ putStrLn "We're doing IO!"
  discountRate' <- asks discountRate
  let discounted = amt * (1 - discountRate' / 100)
  tell $ "-Discount " ++ (show amt) ++ " = " ++ (show discounted)
  return discounted

displayWRIO :: Float -> AppIO String
displayWRIO amt = do
  liftIO $ putStrLn "More IO!"
  currencySym' <- asks currencySym
  tell " > Displaying..."
  return (currencySym' ++ " " ++ (show amt))

