{--
Problem 21
Insert an element at a given position into a list.

Example:

* (insert-at 'alfa '(a b c d) 2)
(A ALFA B C D)
Example in Haskell:

P21> insertAt 'X' "abcd" 2
"aXbcd"
--}

myInsertAt :: a -> [a] -> Int -> [a]
myInsertAt el xs 0 = el:xs
myInsertAt el (x:xs) idx = x : (myInsertAt el (xs) (idx-1))
