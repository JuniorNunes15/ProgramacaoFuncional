{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char (toUpper, toLower)


--upper
upper [] = []
upper (x:xs) = upper' x : upper xs

upper' :: Char -> Char
upper' x = toUpper x


--titulo
titulo (x:xs) = toUpper x : til xs
            where
                til [] = []
                til (x:xs)
                    | x == ' ' = x: toUpper (head xs): til (tail xs)
                    | otherwise = toLower x: til xs


--selec
selec xs [] = []
selec xs (y:ys) = sele xs y : selec xs ys
                    where
                        sele xs 0 = head xs
                        sele (x:xs) y = sele xs (y-1)


--palind
isPalind [] = True
isPalind [x] = True 
isPalind (x:xs)
                | x == head(reverse xs) = isPalind (retire xs)
                | otherwise = False
                    where
                        retire xs = reverse $ tail $ reverse xs
