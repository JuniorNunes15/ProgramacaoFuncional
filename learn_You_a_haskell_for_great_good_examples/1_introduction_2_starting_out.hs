--compiling the chapter 1 e 2 examples

--immutable list of numbers 
xs :: [Integer]
xs = [1,2,3,4,5,6,7,8]

--Baby's firsts functions
doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = x*2 + y*2

doubleUs' :: Num a => a -> a -> a
doubleUs' x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2

doubleSmallNumber' :: (Num a, Ord a) => a -> a
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1


conanO'Brien :: [Char]
conanO'Brien = "It's a-me, Mari... Conan O'Brien!"

boomBangs :: Integral a => [a] -> [[Char]]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] 

length' :: Num a => [t] -> a
length' xs = sum [1 | _ <- xs] 

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]  
