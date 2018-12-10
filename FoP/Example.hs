import Contract.Contract
import Prelude hiding (and)

{-
zcb :: Date -> Double -> Currency -> Contract
zcb t n ccy = when (at t) $ scale (konst n) (one ccy)
-}

c11 = european (date "1200GMT 24 Apr 2003") 
    (
        zcb (mkDate "1200GMT 12 May 2003") 0.4 GBP `and`
        give (zcb (mkDate "1200GMT 12 May 2005") 100 GBP)
    )
