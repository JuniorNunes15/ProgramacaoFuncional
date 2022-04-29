
--splitAt [1,2,3,4,5] 2 = ([1,2], 3, [4,5])
splitAt' xs x = (take x xs , xs !! x, drop (x+1) xs)


splitAtRec xs pos = splitAtRec' xs  pos ([], 0, [])

splitAtRec' [] pos result = result
splitAtRec' (x:xs) pos result 
                        | pos > 0 = splitAtRec' xs (pos-1) (antes++[x],elem,depois)
                        | pos == 0 = splitAtRec' xs (pos-1) (antes, x, depois)
                        | otherwise = splitAtRec' xs (pos-1) (antes, elem, depois++[x])
                        where (antes, elem, depois) = result


splitAtRec'' [] pos = ([], 0, [])
splitAtRec'' (x:xs) pos 
                    | 0 < pos = (x:antes,elem,depois)
                    | pos == 0 = (antes, x, depois)
                    | otherwise = (antes, elem, x:depois)
                    where (antes, elem, depois) = splitAtRec'' xs (pos - 1)


fn ((antes, elem, depois), pos) x 
                                | pos > 0 = ((antes++[x], elem, depois), pos-1)
                                | pos == 0 = ((antes, x, depois), pos-1)
                                | otherwise = ((antes, elem, depois++[x]), pos-1)

splitAtRec''' xs pos = fst $ foldl fn (([], 0, []), pos) xs


fn2 x ((antes, elem, depois), pos)  
                                | pos < 0 = ((x:antes, elem, depois), pos-1)
                                | pos == 0 = ((antes, x, depois), pos-1)
                                | otherwise = ((antes, elem, x:depois), pos-1)

spli xs pos = fst $ foldr fn2 (([], 0, []), (length xs - pos -1)) xs


--swap xs a b = take a xs ++ [xs!!b] ++ take (b) (drop (a+1) xs) ++ [xs!!a]

--swap' xs p1 p2 = inicio ++ [ep2] ++ meio ++ [ep1] ++ fim
----            where (inicio, ep1,resto) = splitAt' xs p1
---                 (meio, ep2, fim) = splitAt' resto(p2 - p1 - 1)

