{--
4 Problem 4
(*) Find the number of elements of a list.

Example in Haskell:

Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13
--}

import MyMethods

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLengthAcc :: [a] -> Int
myLengthAcc xs = myLengthAcc' xs 0
                 where
                   myLengthAcc' [] n = n
                   myLengthAcc' (x:xs) n = myLengthAcc' xs (n + 1)

myLengthFoldLeft :: [a] -> Int
myLengthFoldLeft = myFoldl (\n _ -> n + 1) 0

myLengthFoldLeft' :: [a] -> Int
myLengthFoldLeft' = myFoldl inc1 0 where
                    inc1 n _ = n + 1
