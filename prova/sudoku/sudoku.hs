{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List
import Data.Maybe
import Data.Char

neib :: [a] -> Int -> Int -> [a]
--neib xs index lim = take (index+lim) $ drop (index-lim) xs 
neib xs index lim
                | index - lim <= 0 = take (index+lim+1) xs
                | otherwise = take (index+lim) $ drop (index-lim) xs


exists :: Eq a => a -> [a] -> Bool
exists y xs = length (filter(==y) xs) == 1


--dig2char :: (Eq a, Num a, Enum a) => a -> Char
--dig2char x = intToDigit x
--dig2char x = toEnum x
--dig2char :: (Eq a, Num a, Enum a) => a -> Char
--dig2char x = x + '0'

---fit :: (String, Int) ->  Int -> Int -> Bool
fit (xs, lim) index value
                        | (length $ filter(==value)(neib xs index lim)) > 0 = False
                        | otherwise = True


getHoles :: String -> [Int]
getHoles xs = getHoles' xs 0

getHoles' [] _ = []
getHoles' (x:xs) ind
            | x == '.' = ind: getHoles' xs (ind+1)
            | otherwise = getHoles' xs (ind+1)

set (x:xs) 0 value = value : xs
set (x:xs) index value = x : set xs (index-1) value


solve (xs, lim) holes hindex =
    