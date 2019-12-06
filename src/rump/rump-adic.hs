module Main (main) where

import Data.ExactReal

-- reference: https://tore.tuhh.de/bitstream/11420/318/1/Ru88a.pdf
f :: ExactReal -> ExactReal -> ExactReal
f a b = x + y + z + w
    where
        x = 333.75 * b ^ 6
        y = a ^ 2 * (y1 - y2 - y3 - y4)
        y1 = 11 * a ^ 2 * b ^ 2
        y2 = b ^ 6
        y3 = 121 * b ^ 4
        y4 = 2
        z = 5.5 * b ^ 8
        w = a / (2 * b)


main :: IO ()
main = do
    putStrLn $ show $ f a b
        where
            a = 77617.0
            b = 33096.0



