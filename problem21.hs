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


myInsertAt' :: a -> [a] -> Int -> [a]
myInsertAt' el xs idx = snd $ foldr builder (0, []) xs
                          where
                            builder :: [b] -> (Int, [b]) -> (Int, [b])
                            builder crt acc = if fst acc == idx
                                              then ((fst acc) + 1, el:crt:(snd acc))
                                              else ((fst acc) + 1, crt:(snd acc))
