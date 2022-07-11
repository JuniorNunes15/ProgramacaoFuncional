{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--compac
compac [] = []
compac (x:xs)
            | conte x (x:xs) > 1 = [x,conte x (x:xs)]: compac (retire x (x:xs))
            | otherwise = [x]: compac xs

            where
                conte n [] = 0
                conte n (x:xs)
                            | n == x = 1 + conte n xs
                            | otherwise = 0
                retire n [] = []
                retire n (x:xs)
                            | n == x = retire n xs
                            | otherwise = (x:xs)


--vetFib
fib 0 = 0
fib 1 = 0
fib 2 = 1
fib x = fib(x-1) + fib(x-2)

vetFib 0 = []
vetFib x = vetFib (x-1) ++ [fib x]


--ordenada
ordenada [x] = True
ordenada (x:y:xs)
            | x <= y = ordenada (y:xs)
            | otherwise = False


--inserir ordenada
inserir a [] = [a]
inserir a (x:xs)
                | a >= x = x: inserir a xs
                | otherwise = a:x:xs


--qsort



--merge
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
                | x <= y = x : merge xs (y:ys)
                | otherwise = y : merge (x:xs) ys
