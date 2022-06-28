import Data.List
import Data.Maybe


myElemIndex y xs = myIndex' y xs 0
                where
                    myIndex' y [] cont = Nothing
                    myIndex' y (x:xs) cont
                                        | y == x = Just cont
                                        | otherwise = myIndex' y xs (cont+1)


somaMaybe a b
            | isJust a && isJust b = Just $ fromJust a + fromJust b
            | isNothing a = b
            | isNothing b = a
            | otherwise = Nothing


filterMaybe [] = []
filterMaybe (x:xs)
                | isJust x = fromJust x :  filterMaybe xs
                | otherwise = filterMaybe xs
