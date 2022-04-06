--Recursive
countNeg [] = 0
countNeg (x:xs) 
            | x < 0 = 1 + countNeg xs
            | otherwise = countNeg xs

--Not recursive
countNeg' xs = length $ filter(<0) xs


--recursive
final x xs = final' (length(xs) - x) xs
    where final' 0 ys = ys 
          final' x (y:ys) = final' (x-1) ys 

--not recursive
final2 x xs = reverse $ take x $ reverse xs


contarIguais a b c 
                | a == b && b == c = 3
                | a == b || b == c || c == a = 2
                | otherwise = 0


interior (x:xs) = take (length(xs) - 1) xs


gangorra p1 c1 p2 c2 
                | p1 * c1 == p2 * c2 = 0
                | p1 * c1 > p2 * c2 = -1
                | otherwise = 1


min2 x y 
        | x < y = x
        | otherwise = y


min3 x y z 
        | min2 x y > min2 y z = min2 y z
        | otherwise = min2 x y


soma2 x y = x + y