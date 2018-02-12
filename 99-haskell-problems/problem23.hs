{--
Extract a given number of randomly selected elements from a list.

Example:

* (rnd-select '(a b c d e f g h) 3)
(E D A)
Example in Haskell:

Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
eda
--}

import System.Random
{-- cabal install random --}

myRndSelect :: RandomGen g => [a] -> Int -> g -> [a]
myRndSelect xs n g = take n [ xs !! x | x <- randomRs (0, maxSize) g]
                      where
                        maxSize = length xs - 1

{-- runhaskell problem23.hs --}
main = do
  g <- newStdGen
  print (myRndSelect "abcde" 3 g)
