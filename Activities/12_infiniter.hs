
repeat' x = x: repeat' x


replicate' x y = take x $ repeat' y


gerador1 = 0: get 1
            where
                get x = [x,-x] ++ get (x+1)

--gerador2

gerador3 = get'' 1
                where
                    get'' cont = cont : get'' (cont+cont)


expoentes x y
            | mod x y == 0 = 1 + expoentes (div x y) y
            | otherwise = 0


--factors

 
decompor x 
        | x < 10 = [x]
        | otherwise = decompor (div x 10) ++ [mod x 10] 


--base