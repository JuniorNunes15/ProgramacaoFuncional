{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- !!
--recursive
elemento :: Int -> [a] -> a
elemento 0 xs = head xs
elemento y (x:xs)
            | y >= 0 = elemento (y - 1) xs
            | otherwise = elemento (length(x:xs) + y) (x:xs)
--not recursive
elemento' :: Int -> [a] -> a
elemento' y xs
            | y >= 0 = xs !! y
            | otherwise = xs !! (length xs + y)


--elem
--recursive
pertence :: Eq t => t -> [t] -> Bool
pertence _ [] = False
pertence y (x:xs)
                | y == x = True
                | otherwise = pertence y xs
--not recursive
pertence' :: (Foldable t, Eq a) => a -> t a -> Bool
pertence' y xs = y `elem` xs


--lenght
--recursive
length' :: Num p => [a] -> p
length' [] = 0
length' (_:xs) = 1 + length' xs
--not recursive
length'' :: (Foldable t, Num b) => t a -> b
length'' xs = foldr (\ _ -> (+) 1) 0 xs


--maximum
maximum' :: Ord a => [a] -> a
maximum' [x] = x
maximum' xs = max xs
            where
                max (x:y:xs)
                    | x > y = maximum' (x:xs)
                    | otherwise = maximum' (y:xs)


--init
--recursive
init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x: init' xs
--not recursive
init'' :: [a] -> [a]
init'' xs = reverse $ tail $ reverse xs


--splitAt
divide :: [a] -> Int -> [[a]]
divide xs x = take x xs : [drop x xs]


--partition
splitints :: (a -> Bool) -> [a] -> [[a]]
splitints x xs = [(filter x xs)] ++ [(filter (not . x) xs)]


--somaImpares
--recursive
somaImpares :: Integral a => [a] -> a
somaImpares [] = 0
somaImpares (x:xs)
                | mod x 2 == 0 = somaImpares xs
                | otherwise = x + somaImpares xs
--not recursive
somaImpares' :: Integral a => [a] -> a
somaImpares' xs = sum $ filter odd xs


--max3
max3 :: Ord a => a -> a -> a -> a
max3 a b c
        | a >= b && a >= c = a
        | b >= a && b >= c = b
        | otherwise = c


--fatorial
--recursive
fatorial :: (Eq p, Num p) => p -> p
fatorial 0 = 1
fatorial x = x * fatorial (x-1)
--not recursive
fatorial' :: (Num a, Enum a) => a -> a
fatorial' x = product [1..x]


--uniao
uniao :: Eq a => [a] -> [a] -> [a]
uniao [] ys = ys
uniao (x:xs) ys = x: uniao xs (filter(/=x) ys)


--intersec
--recursive
intersec :: Eq a => [a] -> [a] -> [a]
intersec [] ys = []
intersec (x:xs) ys = (filter(== x) ys)++ intersec xs ys
--not recursive
intersec' :: (Foldable t, Eq a) => t a -> [a] -> [a]
intersec' xs ys = foldr (\ x -> (++) (filter (== x) ys)) [] xs


--sublist
sublist :: Int -> Int -> [a] -> [a]
sublist x y xs
        | x >= 0 && y >= 0 = drop x $ take y xs
        | x >= 0 && y <= 0 = drop x $ take (length xs +y) xs
        | x <= 0 && y >= 0 = drop (length xs + x) $ take y xs
        | otherwise = drop (length xs + x) $ take (length xs +y) xs


--paridade
paridade :: [Bool] -> Bool
paridade xs
        | mod(length $ filter(== True) xs) 2 == 1 = True
        | otherwise =  False


--swap
swap :: [a] -> Int -> Int -> [a]
swap xs x y 
        | y >= length xs || x >= length xs = xs
        | otherwise = take x xs ++ [xs !! y] ++ drop (x+1) (take y xs) ++ [xs!!x] ++ drop (y+1) xs


--euler
euler1 :: Integral a => a -> a
euler1 x = euler2 [0..(x-1)]
        where
          euler2 [] = 0
          euler2 (y:ys)
                | mod y 3 == 0 = y + euler2 ys
                | mod y 5 == 0 = y + euler2 ys
                | otherwise = euler2 ys
