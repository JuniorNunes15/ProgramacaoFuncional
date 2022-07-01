import Control.Monad (replicateM)
--pedras na lua

toInt x = read x :: Int

--desenpacota line = (a, b)
--    where 
---     [a, b] = map toInt . words $  line

processa vet = colar 0 (pro' $ ind(pro vet))
            where
                pro [] = []
                pro (x:vet) = words ((x:vet)!!0) ++ pro vet

                ind [] = []
                ind (x:xs) = toInt x : ind xs

                pro' [] = []
                pro' (x:y:xs) = [(x,y)] ++ pro' (xs)

                colar ind [] = []
                colar ind (x:xs) = [(ind,x)] ++ colar (ind+1) xs


processa' vet = ganhador (proa' 0 $ ind(proa vet))
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
                        |otherwise = gan xs

                gan (ind,(a,b)) (ind2, (c, d))


main :: IO ()
main = do
    size <- readLn :: IO Int
    vet <- replicateM size getLine
    print $ processa' vet

