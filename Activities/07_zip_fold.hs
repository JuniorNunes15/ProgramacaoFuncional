{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


--produto escalar
produtoEscalar xs ys = calculo (zip xs ys)
                where
                    calculo [] = 0
                    calculo (x:xs) = mult x + calculo xs
                    mult (a,b) = a*b
                                                

--indices
indices v xs = find $ zip xs [0..]
                where
                    find [] = []
                    find (x:xs)
                                | fst x == v = snd x : find xs
                                | otherwise = find xs


--concatMap apenas com map
concatMap' xs ys = cd $ map xs ys
                    where
                        cd [] = []
                        cd (x:xs) = x++ cd xs


--eh primo
ehPrimo x 
        | x == 2 || x == 3 || x == 5 || x == 7 || x == 11 = True
        | foldl mod x [2,3,5,7,11] == 0 = False
        | otherwise = True


--vigenere



--mudancaDeBase 



--reduce
divide (a,b) x
                | mod a x == 0 && mod b x == 0 = divide (div a x, div b x) x
                | otherwise = (a,b)

reduce (a,b) = foldl divide (a,b) [11,7,5,3,2]
