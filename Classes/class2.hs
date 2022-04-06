{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

iguais :: (Eq a, Num p) => a -> a -> a -> p
iguais a b c 
           | a == b && a == c = 3
           | a == b || b == c || a == c = 2
           | otherwise = 0

gangorra :: (Num a, Num p, Ord a) => a -> a -> a -> a -> p
gangorra p1 c1 p2 c2 
                    | p1 * c1 == p2 * c2 = 0
                    | p1 * c1 > p2 * c2 = -1
                    | otherwise = 1

length' [] = 0
length' (x:xs) = 1+length' xs

