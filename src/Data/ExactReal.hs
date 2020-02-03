module Data.ExactReal (
    ExactReal (..)
) where

import Data.ExactReal.Internal
import Data.Ratio (numerator, denominator)

newtype ExactReal = ER AdicFloat

instance Show ExactReal where
    show (ER x) = afDec2 16 x

instance Num ExactReal where
    (ER x) + (ER y) = ER $ afAdd x y
    (ER x) - (ER y) = ER $ afSub x y
    (ER x) * (ER y) = ER $ afMul x y
    negate (ER x) = ER $ afNegate x
    fromInteger i = ER $ decintAf $ show i
    abs (ER x) = undefined
    signum (ER x) = undefined

instance Fractional ExactReal where
    (ER x) / (ER y) = ER $ afDiv x y
    fromRational n = ER $ afIntDiv (decintAf . show $ numerator n) (denominator n)

