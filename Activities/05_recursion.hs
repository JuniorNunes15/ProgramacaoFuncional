{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

--replicate
replicate' :: (Eq t, Num t) => t -> a -> [a]
replicate' 0 _ = []
replicate' x y = y : replicate' (x-1) y


--fibonacci
fib :: (Eq a, Num a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib x = fib(x-1) + fib(x-2)


--frequencia
frequencia :: (Num p, Eq t) => t -> [t] -> p
frequencia y [] = 0
frequencia y (x:xs) 
                | y == x = 1 + frequencia y xs
                | otherwise = frequencia y xs


--unico
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


--maiores que
maioresQue :: Ord a => a -> [a] -> [a]
maioresQue _ [] = []
maioresQue y (x:xs) 
            | x > y = x: maioresQue y xs
            | otherwise = maioresQue y xs


--concatena
concatena :: [a] -> [a] -> [a]
concatena [] ys = ys
concatena xs [] = xs
concatena (x:xs) (y:ys) = x: concatena xs (y:ys)


--alter
alter :: (Eq a, Num a) => a -> [a]
alter 0 = []
alter x = alter (x-1) ++ [x, -x]


--reverso
reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]


--menores
menores :: Ord a => Int -> [a] -> [a]
menores x xs 
            | x <= length xs = reverse $ filterMenor (length xs - x) (reverse xs)
            | otherwise = xs
            where
                filterMenor 0 xs = xs
                filterMenor x xs = filterMenor (x-1) (deletee (maior xs) xs) --deleta a primeira ocorrencia do maior elemento da lista e chama recursivamente até ter tirado todos os maiores

                maior [x] = x --pega o maior elemento da lista
                maior (x:y:xs)
                        | y > x = maior (y:xs)
                        | otherwise = maior (x:xs)


--remover o maior elemento
removerMaior :: Ord a => [a] -> [a]
removerMaior [_] = []
removerMaior (x:y:xs) 
                    | x > y = y: removerMaior (x:xs)
                    | otherwise = x: removerMaior (y:xs)


--intercal intercala duas listas
intercal :: [a] -> [a] -> [a]
intercal xs [] = xs
intercal [] ys = ys
intercal (x:xs) (y:ys) = x:y: intercal xs ys


--sequencia
sequencia :: (Eq t, Num t, Num a) => t -> a -> [a]
sequencia 0 _ = []
sequencia x y = y: sequencia (x-1) (y+1)


--rotEsq
rotEsq :: (Eq t, Num t) => t -> [a] -> [a]
rotEsq 0 xs = xs
rotEsq y (x:xs) = rotEsq (y-1) (xs++[x]) 


--rotDir
rotDir :: (Eq t, Num t) => t -> [a] -> [a]
rotDir 0 xs = xs
rotDir y xs = rotDir (y-1) ([head $ reverse xs] ++ take ((length xs) - 1) xs)


--quadperf
quadperf x = quadperf2 x 1
        where
            quadperf2 x y
                        | y*y < x = quadperf2 x (y+1)
                        | y*y == x = True
                        | otherwise = False


--deletee
deletee :: Eq t => t -> [t] -> [t]
deletee _ [] = []
deletee y (x:xs)
                | y == x = xs
                | otherwise = x: deletee y xs


--listacc
listacc :: Num a => [a] -> [a]
listacc [] = []
listacc [x] = [x]
listacc (x:y:xs) = x: listacc (x+y:xs)


--line
line x = lai'' x (line' x)
        where
            lai'' 0 _ = []
            lai'' y x = x : lai'' (y-1) (x+1) 

            line' 0 = 0
            line' 1 = 1
            line' n = (n-1)+ line' (n-1)


--triangle
triangle :: (Eq a, Num a) => a -> [[a]]
triangle 0 = []
triangle x = triangle(x-1) ++ [line x]


--decompor
separa x 
        | x < 10 = [x]
        | otherwise = separa(div x 10) ++ [mod x 10]
