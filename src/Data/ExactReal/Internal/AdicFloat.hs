module Data.ExactReal.Internal.AdicFloat where

import Data.Int
import Data.Char
import Data.ExactReal.Internal.AdicStream

type AdicFloat = (Integer, AdicStream) -- (exponential, mantissa)

---------- Constants ----------

afZero :: AdicFloat
afZero = (0, asZero)

afOne :: AdicFloat
afOne = (1, (1:asZero))

afTen :: AdicFloat
afTen = (1, (10:asZero))



---------- Operators ----------

afAdd :: AdicFloat -> AdicFloat -> AdicFloat
afAdd (e1, m1) (e2, m2) | e1 < e2   = (e2+1, asAdd (shr m1 (e2-e1+1)) (0:m2))
                        | otherwise = (e1+1, asAdd (0:m1) (shr m2 (e1-e2+1)))
    where
        shr xs 0 = xs
        shr xs n = 0 : shr xs (n-1)

afNegate :: AdicFloat -> AdicFloat
afNegate (e, m) = (e, asNegate m)

afSub :: AdicFloat -> AdicFloat -> AdicFloat
afSub a b = afAdd a (afNegate b)

afMul :: AdicFloat -> AdicFloat -> AdicFloat
afMul (e1, m1) (e2, m2) = (e1 + e2, asMul m1 m2)

afDiv :: AdicFloat -> AdicFloat -> AdicFloat
afDiv (e, m) b = afDiv' (e+1, 0:m) (afNorm b)
afDiv' (e1, m1) (e2, m2) = (e1 - e2, asDiv m1 m2)

afIntDiv :: AdicFloat -> Integer -> AdicFloat
afIntDiv _ 0 = undefined
afIntDiv (e, m) n = (e, asIntDiv m n)

afIntPow :: AdicFloat -> Integer -> AdicFloat
afIntPow _ 0 = afOne
afIntPow x 1 = x
afIntPow x n | even n    = afIntPow (afMul x x) (n `quot` 2)
             | otherwise = afMul x $ afIntPow (afMul x x) (n `quot` 2)



---------- Utils ----------

afNorm :: AdicFloat -> AdicFloat
afNorm (e, 0:x) = afNorm (e-1, x)
afNorm a = a

-- did not allow negative exponent
afNormWithZero :: AdicFloat -> AdicFloat
afNormWithZero a@(0, _) = a
afNormWithZero (e, 0:x) | e > 0 = afNormWithZero (e-1, x)
afNormWithZero a = a


---------- for debug ----------

afDec' :: Int -> AdicFloat -> Double
afDec' n (e, a@(x:xs)) | e <= 0    = fromIntegral adic ^^ e * (asDec' 1 $ take n a)
                       | otherwise = fromIntegral (x * adic ^ (e-1)) + afDec' n (e-1, xs)

afDec :: Int -> AdicFloat -> String
afDec n a = show $ afDec' n $ afNormWithZero a


splitComma :: String -> (String, String)
splitComma []    = ([], [])
splitComma ['.'] = undefined
splitComma (x:xs) | x == '.'  = ([], xs)
                  | otherwise = ((x:x'), y')
    where (x', y') = splitComma xs

decintAf :: String -> AdicFloat
decintAf x = afNormWithZero $ decintAf' (reverse x)
decintAf' [] = afZero
decintAf' (x:xs) = afAdd (afMul afTen (decintAf' xs)) (1, ctoi x : asZero)
    where ctoi x = fromIntegral $ ord x - ord '0'

decAf :: String -> AdicFloat
decAf ('+':x) = afAdd (decintAf x') (0, decAs' y')
    where (x', y') = splitComma x
decAf ('-':x) = afNegate $ afAdd (decintAf x') (0, decAs' y')
    where (x', y') = splitComma x
decAf      x  = afAdd (decintAf x') (0, decAs' y')
    where (x', y') = splitComma x
