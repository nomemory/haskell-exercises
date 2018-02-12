{--
(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

Example in Haskell:

*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True
--}

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' xs = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)
