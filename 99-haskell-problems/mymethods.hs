module MyMethods
(
 myFoldl ,
 myFlip ,
 myConcatMap ,
 myGroup,
 myRepeat
)
where

{--
"Academical" implementations of useful methods.
--}

{--
TODO Implement foldr
--}

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ acc [] = acc
myFoldl func acc (x:xs) = myFoldl func (func acc x) xs

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f = g
            where
              g x y = f y x

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = foldr ((++) . f) []

myGroup :: (Eq a) => [a] -> [[a]]
myGroup = foldr grouper []
            where
              grouper x [] = [[x]]
              grouper el ((x:xs):ys)
                | el == x = ((el:x:xs):ys)
                | otherwise = ([el]:(x:xs):ys)

myRepeat ::  Int -> a -> [a]
myRepeat 0 a = []
myRepeat x a = a : (myRepeat (x-1) a)
