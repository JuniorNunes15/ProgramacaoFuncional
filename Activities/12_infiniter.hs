{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

repeat' x = x: repeat' x


replicate' x y = take x $ repeat' y


gerador1 = 0: get 1
            where
                get x = [x,-x] ++ get (x+1)

gerador2 = bb [x | x <- [1,3..]] [y | y <- [-2,-4..]]
        where
            bb (x:xs) (y:ys) = x:y: bb xs ys

gerador3 = get'' 1
                where
                    get'' cont = cont : get'' (cont+cont)



expoentes x y = primeiro $ last $ take 100 $ expe x y 0 
        where
            primeiro (x, y, z) = x

expe x y cont
            | mod x y == 0 = (cont+1,x, mod x y) : expe (div x y) y (cont+1)
            | otherwise = (cont, x, mod x y) : expe x y cont



--factors



decompor x = reverse $ take (pega x) $ dec x
        where 
            pega 0 = 0
            pega x = 1+ pega (div x 10)
dec x 
        | x < 10 = [x] ++ dec (div x 10)
        | otherwise = [mod x 10] ++ dec (div x 10)


--base