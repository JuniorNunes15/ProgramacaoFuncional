

--sdig soma dos digitos
sdig x 
    | x < 10 = x
    | otherwise = (mod x 10) +  sdig (div x 10)


--rev reverter inteiro
rev x = rev' x 0
            where
                rev' x cont
                    | x < 10 = cont + x
                    | otherwise = rev' (div x 10) ((cont+(mod x 10))*10)


--base mudanca de base
base x y 
        | x < y = [dx !! x]
        | otherwise = base (div x y) y ++ [dx !! (mod x y)]
            where
                dx = ['0'..'9']++['A'..'Z'] 

