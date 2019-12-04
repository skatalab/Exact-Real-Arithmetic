module Main (main) where

import Prelude hiding ((+), (-), (*), (/), (^))
import Data.AdicFloat

(+), (-), (*), (/) :: AdicFloat -> AdicFloat -> AdicFloat
(+) = afAdd
(-) = afSub
(*) = afMul
(/) = afDiv

(^) :: AdicFloat -> Integer -> AdicFloat
(^) = afIntPow

-- reference: https://tore.tuhh.de/bitstream/11420/318/1/Ru88a.pdf
f :: AdicFloat -> AdicFloat -> AdicFloat
f a b = x + y + z + w
    where
        x = (decAf "333.75") * (b ^ 6)
        y = (a ^ 2) * (y1 - y2 - y3 - y4)
        y1 = (decAf "11") * (a ^ 2) * (b ^ 2)
        y2 = b ^ 6
        y3 = (decAf "121") * (b ^ 4)
        y4 = decAf "2"
        z = (decAf "5.5") * (b ^ 8)
        w = a / ((decAf "2") * b)


main :: IO ()
main = do
    putStrLn $ afDec 3 $ f a b
        where
            a = decAf "77617.0"
            b = decAf "33096.0"



