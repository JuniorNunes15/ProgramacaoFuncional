{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

--repeat
repeat' x = x: repeat' x


--replicate
replicate' x y = take x $ repeat' y


--geradores
gerador1 = 0: get 1
            where
                get x = [x,-x] ++ get (x+1)

gerador2 = bb [x | x <- [1,3..]] [y | y <- [-2,-4..]]
        where
            bb (x:xs) (y:ys) = x:y: bb xs ys

gerador3 = get'' 1
                where
                    get'' cont = cont : get'' (cont+cont)


--expoentes
expoentes x y = primeiro $ last $ take x $ expe x y 0 
        where
            primeiro (x, y, z) = x

expe x y cont
            | mod x y == 0 = (cont+1,x, mod x y) : expe (div x y) y (cont+1)
            | otherwise = (cont, x, mod x y) : expe x y cont


--factors
factors x = filtra $ geraPrimo x 0 0 0 0 0
            where
                filtra [] = []
                filtra ((x,y):xs)
                                | y /= 0 = (x,y): filtra xs
                                | otherwise = filtra xs
                geraPrimo x p1 p2 p3 p4 p5
                        | mod x 2 == 0 = geraPrimo (div x 2) (p1+1) p2 p3 p4 p5
                        | mod x 3 == 0 = geraPrimo (div x 3) p1 (p2+1) p3 p4 p5
                        | mod x 5 == 0 = geraPrimo (div x 5) p1 p2 (p3+1) p4 p5
                        | mod x 7 == 0 = geraPrimo (div x 7) p1 p2 p3 (p4+1) p5
                        | mod x 11 == 0 = geraPrimo (div x 11) p1 p2 p3 p4 (p5+1)
                        | x > 11 = [(2,p1),(3,p2),(5,p3),(7,p4),(11,p5),(x,1)]
                        | otherwise = [(2,p1),(3,p2),(5,p3),(7,p4),(11,p5)]



--decompor
decompor x = reverse $ take (pega x) $ dec x
        where 
            pega 0 = 0
            pega x = 1+ pega (div x 10)
dec x 
        | x < 10 = [x] ++ dec (div x 10)
        | otherwise = [mod x 10] ++ dec (div x 10)


--base
base :: Int -> Int -> [Char]
base x y = reverse $ take(num x y) $ base' x y
            where
                num 0 y = 0
                num x y = 1 + num (div x y) y

base' :: Int -> Int -> [Char]
base' x y = [dx !! (mod x y)] ++ base' (div x y) y
            where
                dx = ['0'..'9']++['A'..'Z'] 
                


