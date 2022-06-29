

sdig x 
    | x < 10 = x
    | otherwise = (mod x 10) +  sdig (div x 10)


rev x = rev' x 0
            where
                rev' x cont
                    | x < 10 = cont + x
                    | otherwise = rev' (div x 10) ((cont+(mod x 10))*10)

--coverte a b

