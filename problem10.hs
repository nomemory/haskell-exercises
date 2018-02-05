{--
10 Problem 10
(*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example:

* (encode '(a a a a b c c a a d e e e e))
((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
Example in Haskell:

encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
--}

import MyMethods

myEncode :: Eq a => [a] -> [(Int, a)]
myEncode = foldr grouper []
            where
              grouper el [] = [(1, el)]
              grouper el ((num, val):xs)
                | el == val = ((num + 1, val):xs)
                | otherwise = ((1, el):(num, val):xs)

myEncode' :: Eq a => [a] -> [(Int, a)]
myEncode' xs = map pair $ myGroup xs
                where
                  pair lst = (length lst, head lst)
