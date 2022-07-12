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



-- prepara a entrada para a função recursiva de resolução
mainSolver :: String -> Int -> String
mainSolver xs lim = ...

main :: IO ()
main = do
    xs <- getLine
    lim <- readLn :: IO Int
    putStrLn $ mainSolver xs lim


------------------------------------------------------------------------------------
-- Testes para lhe auxiliar a construir a solução de forma mais segura
------------------------------------------------------------------------------------


neibTest :: IO ()
neibTest = do
    print $ neib "abcdef.." 0 2 == "abc"
    print $ neib "abc.def"  3 1 == "c.d"
    print $ neib "abc.def"  3 2 == "bc.de"
    print $ neib "abc.def"  1 2 == "abc."
    print $ neib "abc.def"  5 3 == "c.def"

dig2charTest :: IO ()
dig2charTest = do
    print $ map dig2char [0..9] == ['0'..'9']

setTest :: IO ()
setTest = do
    print $ set "12345" 0 9 == "92345"
    print $ set "12345" 1 9 == "19345"
    print $ set "12345" 4 9 == "12349"

fitTest :: IO ()
fitTest = do -- (fit ("12.345", 1) 2) se torna uma função curry faltando só uma var que seria o valor recebido do vetor
    print $ map (fit ("12.345", 1) 2) [1,2,3,4,5] == [True, False, False, True, True]
    print $ map (fit ("12.345", 2) 2) [1,2,3,4,5] == [False, False, False, False, True]
    print $ map (fit ("12.345", 3) 2) [1,2,3,4,5] == [False, False, False, False, False]
    print $ map (fit ("12345.", 4) 5) [1,2,3,4,5] == [True, False, False, False, False]

getHolesTest :: IO ()
getHolesTest = do
    print $ getHoles "12.3.." == [2,4,5]
    print $ getHoles "12.3.4" == [2,4]
    print $ getHoles "...3.4" == [0,1,2,4]

mainTest :: IO ()
mainTest = do
    print $ mainSolver "01.2." 3 == "01320"
    print $ mainSolver ".0..231..5" 5 == "1045231045"
    print $ mainSolver "2..0..............3..........." 3 == "213021302130213021302130213021"
    print $ mainSolver "0..32..41." 5 == "0413250413"
    print $ mainSolver "9....7.620.5318....." 9 == "95318746209531874620"

