{--

Problem 2
(*) Find the last but one element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'

--}

myButLast :: [a] -> a
myButLast [] = error "Empty List."
myButLast [x] = error "Only one element in the list."
myButLast (x:_:[]) = x
myButLast (x:xs) = myButLast xs
