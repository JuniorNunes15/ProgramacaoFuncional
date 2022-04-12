{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

replicate' :: (Eq t, Num t) => t -> a -> [a]
replicate' 0 _ = []
replicate' x y = y : replicate' (x-1) y


fib :: (Eq a, Num a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib x = fib(x-1) + fib(x-2)



frequencia :: (Num p, Eq t) => t -> [t] -> p
frequencia y [] = 0
frequencia y (x:xs) 
                | y == x = 1 + frequencia y xs
                | otherwise = frequencia y xs


unico :: Eq t => t -> [t] -> Bool
unico x xs 
        | (contUnico x xs) > 1 = False
        | (contUnico x xs) < 1 = False
        | otherwise = True
            where
                contUnico y [] = 0
                contUnico y (x:xs)
                            | y == x = 1+ contUnico y xs
                            | otherwise = contUnico y xs


maioresQue :: Ord a => a -> [a] -> [a]
maioresQue _ [] = []
maioresQue y (x:xs) 
            | x > y = x: maioresQue y xs
            | otherwise = maioresQue y xs


concatena :: [a] -> [a] -> [a]
concatena [] ys = ys
concatena xs [] = xs
concatena (x:xs) (y:ys) = x: concatena xs (y:ys)


alter :: (Eq a, Num a) => a -> [a]
alter 0 = []
alter x = alter (x-1) ++ [x, -x]


reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]


--menores


removerMaior :: Ord a => [a] -> [a]
removerMaior [_] = []
removerMaior (x:y:xs) 
                    | x > y = y: removerMaior (x:xs)
                    | otherwise = x: removerMaior (y:xs)


intercal :: [a] -> [a] -> [a]
intercal xs [] = xs
intercal [] ys = ys
intercal (x:xs) (y:ys) = x:y: intercal xs ys


sequencia :: (Eq t, Num t, Num a) => t -> a -> [a]
sequencia 0 _ = []
sequencia x y = y: sequencia (x-1) (y+1)


rotEsq :: (Eq t, Num t) => t -> [a] -> [a]
rotEsq 0 xs = xs
rotEsq y (x:xs) = rotEsq (y-1) (xs++[x]) 


rotDir :: (Eq t, Num t) => t -> [a] -> [a]
rotDir 0 xs = xs
rotDir y xs = rotDir (y-1) ([head $ reverse xs] ++ take ((length xs) - 1) xs)


quadperf x = quadperf2 x 1
        where
            quadperf2 x y
                        | y*y < x = quadperf2 x (y+1)
                        | y*y == x = True
                        | otherwise = False


deletee :: Eq t => t -> [t] -> [t]
deletee _ [] = []
deletee y (x:xs)
                | y == x = xs
                | otherwise = x: deletee y xs


listacc :: Num a => [a] -> [a]
listacc [] = []
listacc (x:y:xs) = x: listacc (x+y:xs)


--line


