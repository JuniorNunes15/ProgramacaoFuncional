{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

--count neg
--Recursive
countNeg :: (Ord a, Num p, Num a) => [a] -> p
countNeg [] = 0
countNeg (x:xs) 
            | x < 0 = 1 + countNeg xs
            | otherwise = countNeg xs
--Not recursive
countNeg' :: (Ord a, Num a) => [a] -> Int
countNeg' xs = length $ filter(<0) xs


--final
--recursive
final :: Int -> [a] -> [a]
final x xs = final' (length(xs) - x) xs
    where final' 0 ys = ys 
          final' x (y:ys) = final' (x-1) ys 
--not recursive
final2 :: Int -> [a] -> [a]
final2 x xs = reverse $ take x $ reverse xs


--contar iguais
contarIguais :: (Eq a, Num p) => a -> a -> a -> p
contarIguais a b c 
                | a == b && b == c = 3
                | a == b || b == c || c == a = 2
                | otherwise = 0


--interior miolo da lista
interior :: [a] -> [a]
interior (x:xs) = take (length(xs) - 1) xs


--gangorra
gangorra :: (Num a, Num p, Ord a) => a -> a -> a -> a -> p
gangorra p1 c1 p2 c2 
                | p1 * c1 == p2 * c2 = 0
                | p1 * c1 > p2 * c2 = -1
                | otherwise = 1


--min2
min2 :: Ord p => p -> p -> p
min2 x y 
        | x < y = x
        | otherwise = y


--min3
min3 :: Ord p => p -> p -> p -> p
min3 x y z 
        | min2 x y > min2 y z = min2 y z
        | otherwise = min2 x y


--soma2
soma2 :: Num a => a -> a -> a
soma2 x y = x + y
