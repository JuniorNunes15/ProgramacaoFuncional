import Control.Monad (replicateM)
--pedras na lua

toInt x = read x :: Int

--desenpacota line = (a, b)
--    where 
---     [a, b] = map toInt . words $  line

processa vet = vencedor(filtra (colar 0 (pro' $ ind(pro vet))))
            where
                pro [] = []
                pro (x:vet) = words ((x:vet)!!0) ++ pro vet

                ind [] = []
                ind (x:xs) = toInt x : ind xs

                pro' [] = []
                pro' (x:y:xs) = [(x,y)] ++ pro' (xs)

                colar ind [] = []
                colar ind (x:xs) = [(ind,x)] ++ colar (ind+1) xs

                filtra [] = []
                filtra (x:xs) = filt x ++ filtra xs

                filt (a,(b,c))
                            | b < 10 || c < 10 = []
                            | otherwise = [(a,(b,c))]

                vencedor [] = "sem ganhador"
                vencedor (x:[]) = venc x
                vencedor (x:y:xs)
                                | contvenc x < contvenc y = vencedor (x:xs)
                                | otherwise = vencedor (y:xs)

                venc (a,(b,c)) = show a

                contvenc (a,(b,c)) = abs (b-c)


{-processa' vet = ganhador (proa' 0 $ ind(proa vet))
            where
                proa [] = []
                proa (x:vet) = words ((x:vet)!!0) ++ proa vet

                ind [] = []
                ind (x:xs) = toInt x : ind xs

                proa' ind [] = []
                proa' ind (x:y:xs)
                            | x < 10 || y < 10 = (proa' (ind+1) xs)
                            |otherwise = [(ind,(x,y))] ++ (proa' (ind+1) xs)

                ganhador xs
                        | xs == [] = "sem ganhador"
                        | length xs == 1 = "1"
                        | otherwise = ganhador(gan (head xs) (head (tail xs)))

                gan (ind,(a,b)) (ind2, (c, d))
                                        | abs (a-b) > abs (c-d) = show ind2
                                        | otherwise = show ind
-}

main :: IO ()
main = do
    size <- readLn :: IO Int
    vet <- replicateM size getLine
    print $ processa vet

