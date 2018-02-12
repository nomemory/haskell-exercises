{--

9 Problem 19
(**) Rotate a list N places to the left.

Hint: Use the predefined functions length and (++).

Examples:

* (rotate '(a b c d e f g h) 3)
(D E F G H A B C)

* (rotate '(a b c d e f g h) -2)
(G H A B C D E F)
Examples in Haskell:

*Main> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"

*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"

--}

myRotate :: [a] -> Int -> [a]
myRotate [] _ = []
myRotate x 0 = x
myRotate x y
  | y > 0 = myRotate (tail x ++ [head x]) (y-1)
  | otherwise = myRotate (last x : init x) (y+1)
