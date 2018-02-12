{--

Drop every N'th element from a list.

Example:

* (drop '(a b c d e f g h i k) 3)
(A B D E G H K)
Example in Haskell:

*Main> dropEvery "abcdefghik" 3
"abdeghk"

--}

myDropEvery :: [a] -> Int -> [a]
myDropEvery xs n = recurse xs n
                    where
                      recurse :: [a] -> Int -> [a]
                      recurse [] _ = []
                      recurse (x:xs) 1 = recurse xs n
                      recurse (x:xs) n = x : (recurse xs (n-1))
