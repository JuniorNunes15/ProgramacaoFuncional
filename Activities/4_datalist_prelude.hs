{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (nub)

map' y [] = []
map' y (x:xs) = y x : map' y xs



filter' y [] = []
filter' y (x:xs)
                | y x = x : filter' y xs
                | otherwise = filter' y xs



tails [x] = [[x],[]]
tails (x:xs) = (x:xs) : tails xs



nub' [] = []
nub' (x:xs) = x : nub' (filter(/=x) xs) 
