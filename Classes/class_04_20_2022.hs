--4月/20日/2022年
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Distribution.Simple.Utils (xargs)

con :: (Eq a, Num a) => [a] -> a -> [a]
con [a,y] x
        | y == x = [a+1,y]
        | otherwise = [a,y]

contar :: (Foldable t, Eq a, Num a) => a -> t a -> [a]
contar y xs = foldl con [0,y] xs


somar_todos :: Num b => [b] -> b
somar_todos (x:xs) = foldl soma x xs
            where
                soma x y = x + y

menor :: Ord b => [b] -> b
menor (x:xs) = foldl minimum x xs
            where
                minimum x y
                        | x < y = x
                        | otherwise = y


enc :: (Eq a, Num a) => [a] -> a -> [a]
enc [a,x] y 
        | x == y = [a+1,x]
        | otherwise = [a,y]

encontrar_rep :: (Eq a, Num a) => a -> [a] -> a
encontrar_rep a (x:xs) = head(foldl enc [a,x] xs)



      