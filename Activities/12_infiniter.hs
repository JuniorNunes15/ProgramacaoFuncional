
repeat' x = x: repeat' x


replicate' x y = take x $ repeat' y


gerador1 = 0: get 1
            where
                get x = [x,-x] ++ get (x+1)

