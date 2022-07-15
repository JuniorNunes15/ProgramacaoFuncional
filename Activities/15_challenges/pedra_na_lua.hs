
import Control.Monad (replicateM)
--pedras na lua

toInt x = read x :: Int

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


main :: IO ()
main = do
    size <- readLn :: IO Int
    vet <- replicateM size getLine
    print $ processa vet

