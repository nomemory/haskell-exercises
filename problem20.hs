{--
(*) Remove the K'th element from a list.

Example in Prolog:

?- remove_at(X,[a,b,c,d],2,R).
X = b
R = [a,c,d]
Example in Lisp:

* (remove-at '(a b c d) 2)
(A C D)
(Note that this only returns the residue list, while the Prolog version also returns the deleted element.)

Example in Haskell:

*Main> removeAt 2 "abcd"
('b',"acd")

--}

myRemoveAt :: Int -> [a] -> [a]
myRemoveAt 0 (x:xs)  = xs
myRemoveAt idx (x:xs) = x : (myRemoveAt (idx -1) (xs))
