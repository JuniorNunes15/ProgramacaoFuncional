{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Using foldr on tuple" #-}
{-# LANGUAGE FlexibleContexts #-}


--1 versão recursiva
index_of :: (Eq t, Num p) => [t] -> t -> p
index_of xs y
            | filter(==y) xs == [] = -1
            | head xs == y = 0
            | otherwise = 1 + index_of (tail xs) y



--2 versão com fold
find_index :: (Eq a, Num a) => [a] -> a -> [a]
find_index [a,y] x
            | x == y = [a,a]
            | otherwise = [a+1,y]

index_of2 :: Foldable t => t Int -> Int -> Int
index_of2 xs y
            | (head folde_func) >= length xs = -1
            | otherwise = head $ reverse folde_func
                where
                    folde_func = foldl find_index [0,y] xs



--3
ehTriangulo :: (Ord a, Num a) => a -> a -> a -> Bool
ehTriangulo a b c
                | a + b > c && a + c > b && b + c > a = True
                | otherwise = False



--4
ehPoligono :: (Ord a, Num a) => [a] -> Bool
ehPoligono xs
            | maior xs >= (soma xs - maior xs) = False
            | otherwise = True
            where
                maior [x] = x
                maior (x:y:xs)
                            | x > y = maior (x:xs)
                            | otherwise = maior (y:xs)
                soma [] = 0
                soma (x:xs) = x + soma xs



--5
combinacoes :: (Ord a, Num a) => [a] -> [(a, a, a)]
combinacoes [x, y] = []
combinacoes (x:xs) = triangulo x xs ++ combinacoes xs
            where
                triangulo a [x] = []
                triangulo a (x:y:xs)
                                | a + x > y && a + y > x && x + y > a = (a,x,y) : triangulo a (y:xs)
                                | otherwise = triangulo a (y:xs)


