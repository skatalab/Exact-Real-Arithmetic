module Data.ExactReal.Internal.AdicStream where

import Data.Int
import Data.Char
import Data.Bits
import Debug.Trace

type AdicDigit = Integer
type AdicStream = [AdicDigit]


---------- Constants ----------

adic :: AdicDigit
-- adic = 10
adic = 2 ^ 32

asZero :: AdicStream
asZero = 0 : asZero

asOne :: AdicStream
asOne = (adic - 1) : asOne

asMOne :: AdicStream
asMOne = (-adic + 1) : asMOne



---------- Operators ----------

asAdd' :: AdicStream -> AdicStream -> Int -> AdicStream
asAdd' (x1:x2:xs) (y1:y2:ys) carry | x2 + y2 - 1 <= -adic && carry < 0 =   adic + (x1 + y1 - 1)  : nexts (-1)
                                   | x2 + y2 - 1 <= -adic && 0 < carry = -(adic - (x1 + y1 - 1)) : nexts (-1)
                                   | x2 + y2 - 1 <= -adic              =          (x1 + y1 - 1)  : nexts (-1)
                                   | adic <= x2 + y2 + 1  && carry < 0 =   adic + (x1 + y1 + 1)  : nexts 1
                                   | adic <= x2 + y2 + 1  && 0 < carry = -(adic - (x1 + y1 + 1)) : nexts 1
                                   | adic <= x2 + y2 + 1               =          (x1 + y1 + 1)  : nexts 1
                                   |                         carry < 0 =   adic + (x1 + y1)      : nexts 0
                                   |                         0 < carry = -(adic - (x1 + y1))     : nexts 0
                                   | otherwise                         =          (x1 + y1)      : nexts 0
    where nexts = asAdd' (x2:xs) (y2:ys)

-- Result undefined if not in (-1, 1)
asAdd :: AdicStream -> AdicStream -> AdicStream
asAdd a@(x:xs) b@(y:ys) | x + y - 1 <= -adic = mone_plus_posx $ asAdd' a b (-1)
                        | adic <= x + y + 1  = one_plus_negx $ asAdd' a b 1
                        | otherwise          = asAdd' a b 0

asNegate :: AdicStream -> AdicStream
asNegate (x:xs) = -x : asNegate xs

asSub :: AdicStream -> AdicStream -> AdicStream
asSub a b = asAdd a $ asNegate b

asDigitMulNonCarry :: AdicStream -> AdicDigit -> AdicStream
asDigitMulNonCarry (x:xs) n = (n * x) `rem` adic : asDigitMulNonCarry xs n
-- asDigitMulNonCarry (x:xs) n = (0xffffffff .&. fromIntegral (n * x)) : asDigitMulNonCarry xs n

asDigitMulCarry :: AdicStream -> AdicDigit -> AdicStream
asDigitMulCarry (x:xs) n = (n * x) `quot` adic : asDigitMulCarry xs n
-- asDigitMulCarry (x:xs) n = fromIntegral (n * x) `shiftR` 32 : asDigitMulCarry xs n

asDigitMul :: AdicStream -> AdicDigit -> AdicStream
asDigitMul _ 0 = asZero
asDigitMul xs y = asAdd a b
    where
        a = asDigitMulCarry xs y
        b = 0 : asDigitMulNonCarry xs y

asMul :: AdicStream -> AdicStream -> AdicStream
asMul xs (y:ys) = asMul' xs ys (asDigitMul xs y)
asMul' xs (y:ys) hoge = c : asMul' xs ys cs
    where c:cs = asAdd hoge (0:asDigitMul xs y)

-- asDigitDiv' :: AdicStream -> AdicDigit -> AdicStream
-- asDigitDiv' (x1:x2:xs) y = x1 `quot` y : asDigitDiv' nexts y
--     where nexts = (x1 `rem` y) * adic + x2 : xs

-- asDigitDiv :: AdicStream -> AdicDigit -> AdicStream
-- asDigitDiv _ 0 = undefined
-- asDigitDiv (x1:x2:xs) y = asDigitDiv' xss y
--         where xss = (x1 * adic + x2) : xs

asDiv' :: AdicStream -> AdicStream -> AdicStream
asDiv' a@(x1:x2:_) b@(y1:_) = p : asDiv' zs b
    where
        p = (x1 * adic + x2) `quot` y1
        zs = p0tail $ asSub a (asDigitMul b p)

asDiv :: AdicStream -> AdicStream -> AdicStream
asDiv xs ys
    | p0able xs && p0able ys = asDiv (p0tail xs) (p0tail ys)
    | otherwise = asDiv' xs ys

asIntDiv :: AdicStream -> Integer -> AdicStream
asIntDiv x 0 = undefined
asIntDiv x 1 = x
asIntDiv x (-1) = asNegate x
asIntDiv x n = asIntDiv' x n 0

asIntDiv' :: AdicStream -> Integer -> Integer -> AdicStream
asIntDiv' (x:xs) n s = p : asIntDiv' xs n (s' - n * p)
    where
        p = s' `quot` n
        s' = s * adic + x


asIntPow :: AdicStream -> Integer -> AdicStream
asIntPow _ 0 = asOne
asIntPow x 1 = x
asIntPow x n | even n    = asIntPow (asMul x x) (n `div` 2)
             | otherwise = asMul x $ asIntPow x (n - 1)


---------- Utils ----------

-- make starting from 0
-- 1 (-2) => 0 8
-- (-1) 2 => 0 (-8)
p0 :: AdicStream -> AdicStream
p0 x@(0:_) = x
-- p0 (x1:x2:xs) | abs x1 == 1 && signum x1 /= signum x2 = 0 : (x2 - signum x2 * adic) : xs
p0 (1:0:xs) = 0 : (adic - 1) : rest
    where 0:rest = p0 (1:xs)
p0 (1:x2:xs) | x2 < 0 = 0 : (x2 + adic) : xs
p0 (-1:0:xs) = 0 : (-adic + 1) : rest
    where 0:rest = p0 (-1:xs)
p0 (-1:x2:xs) | 0 < x2 = 0:(x2 - adic) : xs
-- p0 _ = undefined
p0 x = trace ("panic! x = " ++ (show $ take 10 x)) undefined

-- tail . p0
p0tail :: AdicStream -> AdicStream
p0tail (0:xs) = xs
p0tail (x:xs) = push x xs

push :: AdicDigit -> AdicStream -> AdicStream
push d (x:xs)
    | abs d' < adic = d' : xs
    | otherwise = (d' - d) : push d xs
    where d' = d * adic + x

p0able :: AdicStream -> Bool
p0able (x1:x2:_) = abs (x1 * adic + x2) < adic

one_plus_negx :: AdicStream -> AdicStream
one_plus_negx (x:xs) | 0 < x  = undefined
                     | x == 0 = (adic - 1) : one_plus_negx xs
                     | x < 0  = (adic + x) : xs

mone_plus_posx :: AdicStream -> AdicStream
mone_plus_posx (x:xs) | 0 < x  = (-adic + x) : xs
                      | x == 0 = (-adic + 1) : mone_plus_posx xs
                      | x < 0  = undefined



---------- for debug ----------

asDec' :: Int -> AdicStream -> Double
asDec' _ [] = 0
asDec' i (x:xs) = (fromIntegral x) * (fromIntegral adic) ^^ (-i) + asDec' (i+1) xs

asDec :: Int -> AdicStream -> String
asDec p xs = show $ asDec' 1 $ take p xs

-- only adic == 10
decAs' :: String -> AdicStream
decAs' [] = asZero
-- decAs' (x:xs) = fromIntegral (ord x - ord '0') : decAs' xs
decAs' x = doubleAs $ read ('0':'.':x)

decAs :: String -> AdicStream
decAs ('-':xs) = asNegate $ decAs xs
decAs ('0':'.':xs) = decAs' xs

doubleAs :: Double -> AdicStream
doubleAs d = p : doubleAs (d * (fromIntegral adic) - (fromIntegral p))
    where p = truncate $ d * (fromIntegral adic)

