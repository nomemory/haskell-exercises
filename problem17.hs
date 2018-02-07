{--
7 Problem 17
(*) Split a list into two parts; the length of the first part is given.

Do not use any predefined predicates.

Example:

* (split '(a b c d e f g h i k) 3)
( (A B C) (D E F G H I K))
Example in Haskell:

*Main> split "abcdefghik" 3
("abc", "defghik")
--}

mySplit :: [a] -> Int -> ([a], [a])
mySplit (x:xs) n | n > 0 = let (f, l) = mySplit xs (n - 1)
                           in (x : f, l)
mySplit xs _ = ([], xs)
