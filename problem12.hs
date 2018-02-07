{--
2 Problem 12
(**) Decode a run-length encoded list.

Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

Example in Haskell:

P12> decodeModified
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
--}

data ListElement a = Single a | Multiple Int a
  deriving Show

decodeModified :: Eq a => [ListElement a] -> [a]
decodeModified = foldr decode []
                  where
                    decode :: ListElement a -> [a] -> [a]
                    decode (Single el) xs = el:xs
                    decode (Multiple 0 el) xs = xs
                    decode (Multiple x el) xs = decode (Multiple (x-1) el) (el:xs)
