{--
2 Problem 22
Create a list containing all integers within a given range.

Example:

* (range 4 9)
(4 5 6 7 8 9)
Example in Haskell:

Prelude> range 4 9
[4,5,6,7,8,9]
--}

myRange :: Int -> Int -> [Int]
myRange x y  = [x..y]

myRange' :: Int -> Int -> [Int]
myRange' x y = myRangeRec x cnt
                  where
                    cnt = y - x
                    myRangeRec x 0 = [x]
                    myRangeRec x cnt = x : (myRangeRec (x+1) (cnt-1))
                    
