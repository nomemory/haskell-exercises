{--
4 Problem 24
Lotto: Draw N different random numbers from the set 1..M.

Example:

* (rnd-select 6 49)
(23 1 17 33 21 37)
Example in Haskell:

Prelude System.Random>diff_select 6 49
Prelude System.Random>[23,1,17,33,21,37]
--}

import qualified Data.Set as Set
import System.Random

type Solution = Set.Set Int

myDiffSelect :: RandomGen g => Int -> Int -> g -> [Int]
myDiffSelect n mLimit g | n >= mLimit = error "Not enough possible results."
                        | otherwise = Set.toList (myDiffSelect' n mLimit g Set.empty)
                          where
                            myDiffSelect' :: (RandomGen g) => Int -> Int -> g -> Solution -> Solution
                            myDiffSelect' 0 mLimit g s = s
                            myDiffSelect' n mLimit g s = let (num, newGen) = randomR (0, mLimit) g in
                                                         if (Set.notMember num s)
                                                            then myDiffSelect' (n-1) mLimit newGen (Set.insert num s)
                                                            else myDiffSelect' n mLimit newGen s


main = do
        g <- getStdGen
        print (myDiffSelect 5 64 g)
