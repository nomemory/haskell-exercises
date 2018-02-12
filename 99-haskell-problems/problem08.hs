{--

8 Problem 8
(**) Eliminate consecutive duplicates of list elements.

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example:

* (compress '(a a a a b c c a a d e e e e))
(A B C A D E)
Example in Haskell:

> compress "aaaabccaadeeee"
"abcade"
--}

import MyMethods

myCompress :: (Eq a) => [a] -> [a]
myCompress = foldr skipIfRep []
              where
                skipIfRep x [] = x:[]
                skipIfRep x acc
                  | (head acc) == x = acc
                  | otherwise = x:acc

myCompress' :: (Eq a) => [a] -> [a]
myCompress' = map head . myGroup
