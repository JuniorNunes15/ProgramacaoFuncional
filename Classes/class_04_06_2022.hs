
--indice

at xs 0 = head xs
at xs y 
        | y >= 0 = at xs (ind - 1)
        | otherwise = at (tail xs) (length(xs) + y)
        where
            ind = mod y (length(xs))
