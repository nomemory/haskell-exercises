{--

Problem 25
Generate a random permutation of the elements of a list.

Example:

* (rnd-permu '(a b c d e f))
(B A D C E F)
Example in Haskell:

Prelude System.Random>rnd_permu "abcdef"
Prelude System.Random>"badcef"

--}

{--
Note: This is not the most efficient solution.
I wanted to create a myExtractAt method and use it in this context.
--}

import System.Random
import System.Environment

data ExtractionAt a = ExtractionAt { extraction :: [a],
                                     firstEls :: [a],
                                     lastEls :: [a]
                                   } deriving Show

myExtractionAtSol :: ExtractionAt a -> (a, [a])
myExtractionAtSol eAt = (head $ extraction eAt, (reverse $ firstEls eAt) ++ (lastEls eAt))

{--
Extracts an element from a given list [a] returning a combination of (el, [a]) where the
second value in the tuple represents the initial list after extraction.

Eg.:

*Main> myExtractAt 3 "abcde"
('d',"abce")
*Main>
--}
myExtractAt :: Show a => Int -> [a] -> (a, [a])
myExtractAt _ [] = error "Empty list, impossible to extract."
myExtractAt idx (x:xs) = if (idx > length xs)
                          then error $ "Invalid index to extract: " ++ (show idx) ++ " in list: " ++ (show xs)
                          else
                            myExtractionAtSol $ f idx (ExtractionAt [x] [] xs)
                              where
                                f :: Int -> ExtractionAt a -> ExtractionAt a
                                f 0 extr = extr
                                f idx (ExtractionAt [e] ys (x:xs)) = f (idx-1) (ExtractionAt (x:[]) (e:ys) xs)


myRandomPermu :: (Show a, RandomGen g) => [a] -> g -> [a]
myRandomPermu [] _ = []
myRandomPermu [x] _ = [x]
myRandomPermu xs g = let
                      (idx, gen) = randomR (0, (length xs)-1) g
                      (el, remaining) = myExtractAt idx xs
                     in
                      el : (myRandomPermu remaining gen)

main = do
        args <- getArgs
        case args of
            [xs] -> do
                g <- getStdGen
                print $ myRandomPermu xs g
            _ -> do print $ "Invalid number of arguments: " ++ (show $ length args) ++ " .Expected: 1."
