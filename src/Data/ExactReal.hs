module Data.ExactReal (
    ExactReal (..)
) where

import Data.ExactReal.Internal
import Data.Ratio (numerator, denominator)

newtype ExactReal = ER AdicFloat

instance Show ExactReal where
    show (ER x) = afDec 2 x

instance Num ExactReal where
    (ER x) + (ER y) = ER $ afAdd x y
    (ER x) - (ER y) = ER $ afSub x y
    (ER x) * (ER y) = ER $ afMul x y
    negate (ER x) = ER $ afNegate x
    fromInteger i = ER $ decAf $ show i
    abs (ER x) = undefined
    signum (ER x) = undefined

instance Fractional ExactReal where
    (ER x) / (ER y) = ER $ afDiv x y
    fromRational n = ER $ afDiv (decAf . show $ numerator n) (decAf . show $ denominator n)

