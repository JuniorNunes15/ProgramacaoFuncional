{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (elemIndex)
import Data.Maybe

--monadas

--maybe, ou é um nothing ou just

purifica :: Num p => Maybe p -> p
purifica Nothing = -1
purifica (Just x) = x

indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = purifica $ elemIndex x xs

-- fromMaybe faz a mesma coisa que o indexOf com o purifica
indexOf' :: Eq a => a -> [a] -> Int
indexOf' x xs = fromMaybe (-1) (elemIndex x xs)


purifica' Nothing Nothing = Nothing
purifica' (Just x) (Just y) = Just $ x + y
purifica' (Just x) Nothing = Just x
purifica' Nothing (Just y) = Just y


soma a b = purifica' a b


soma' :: Maybe Int -> Maybe Int -> Maybe Int
soma' x y
        | isJust x && isJust y = Just $ fromJust x + fromJust y
        | isNothing x && isJust y = y
        | isJust x && isNothing y = x
        | otherwise = Nothing

soma'' :: Maybe Int -> Maybe Int -> Int
soma'' x y
        | isJust x && isJust y = fromJust x + fromJust y
        | isNothing x && isJust y = fromJust y
        | isJust x && isNothing y = fromJust x
        | otherwise = 0



puri xs Nothing = xs
puri xs (Just y) = xs


somaVec xs Nothing = xs 
somaVec xs (Just y) = [x + y | x <- xs]

somaVec' xs my = case my of
                    Nothing -> xs
                    (Just y) -> [x + y | x <- xs]


somaTodos xs = sum [fromMaybe 0 my | my <- xs]
