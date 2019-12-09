# Exact Real Arithmetic

Exact real arithmeric implemented by signed 2^31-adic streams.


## Usage

`import Data.ExactReal`

The `ExactReal` type is an instance of `Num`, `Fractional`, and `Show`.


## Example

```haskell
import Data.ExactReal

f :: ExactReal -> ExactReal -> ExactReal
f a b = x + y + z + w
  where
    x = 333.75 * b ^ 6
    y = a ^ 2 * (y1 - y2 - y3 - 2)
    y1 = 11 * a ^ 2 * b ^ 2
    y2 = b ^ 6
    y3 = 121 * b ^ 4
    z = 5.5 * b ^ 8
    w = a / (2 * b)

main :: IO ()
main = print $ f 77617.0 33096.0
```


## Caution

Division by zero is NOT allowed.
If you do that, occurs an infinite loop.


## References

[Plume, D. (1998). A Calculator for Exact Real Number Computation. Ph.D.Dissertation, University of Edinburgh.](http://www.dcs.ed.ac.uk/home/mhe/plume/index.html)

[Rump, S. M. (1988). Algorithms for Verified Inclusions: Theory and Practice. Reliability in Computing, pp.109-126.](https://tore.tuhh.de/bitstream/11420/318/1/Ru88a.pdf)

<!-- [大石 進一. (2018). 精度保証付き数値計算の基礎. pp.3-5. コロナ社.](https://www.coronasha.co.jp/np/data/tachiyomi/978-4-339-02887-4.pdf) -->

