{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

--map
map' y [] = []
map' y (x:xs) = y x : map' y xs


--filter
filter' y [] = []
filter' y (x:xs)
                | y x = x : filter' y xs
                | otherwise = filter' y xs


--tails
tails [x] = [[x],[]]
tails (x:xs) = (x:xs) : tails xs


--nub
nub' [] = []
nub' (x:xs) = x : nub' (filter(/=x) xs) 
