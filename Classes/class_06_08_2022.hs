import Control.Monad (replicateM)

setM :: [[a]] -> Int -> Int -> a -> [[a]]
setM xxs l c value = 

    




main :: IO ()
main = do
    [nl, nc] <-  map (read :: String -> Int) . words <$> getLine
    mat <- replicateM nl getLine
    mapM_ putStrLn mat 
