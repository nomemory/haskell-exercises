{--

(*) Duplicate the elements of a list.

Example:

* (dupli '(a b c c d))
(A A B B C C C C D D)
Example in Haskell:

> dupli [1, 2, 3]
[1,1,2,2,3,3]

--}

import MyMethods

myDuplicate :: [a] -> [a]
myDuplicate = myDuplicateAcc []
                where
                  myDuplicateAcc acc [] = reverse acc
                  myDuplicateAcc acc (x:xs) = myDuplicateAcc (x:x:acc) xs

myDuplicate' :: [a] -> [a]
myDuplicate' = foldr duplicater []
                where
                  duplicater :: a -> [a] -> [a]
                  duplicater el lst = el:el:lst

myDuplicateCMap :: [a] -> [a]
myDuplicateCMap = myConcatMap (myRepeat 2)
