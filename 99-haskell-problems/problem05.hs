{-- Problem 5
(*) Reverse a list.

Example in Haskell:

Prelude> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> myReverse [1,2,3,4]
[4,3,2,1]
--}

import MyMethods

myReverse :: [a] -> [a]
myReverse = foldl revFlip []
            where
              revFlip x xs = xs:x

myReverse' :: [a] -> [a]
myReverse' = myFoldl (myFlip (:)) []

myReverse''' :: [a] -> [a]
myReverse''' list = myReverseAcc list []
               where
                 myReverseAcc :: [a] -> [a] -> [a]
                 myReverseAcc [] rev = rev
                 myReverseAcc (x:xs) rev = myReverseAcc xs (x:rev)
