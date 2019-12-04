{-# OPTIONS_GHC -Wall #-}

fuel :: Int -> Int
fuel m = m `div` 3 - 2

fuel' :: Int -> Int
fuel' m = sum $ takeWhile (>0) (iterate f (f m))
  where f x = x `div` 3 - 2

solve :: [Int] -> Int
solve = sum . map fuel

solve' :: [Int] -> Int
solve' = sum . map fuel'

main :: IO ()
main = do
    c <- getContents
    let ms = map read (words c)
    putStrLn $ "Part One: " ++ show (solve  ms)
    putStrLn $ "Part Two: " ++ show (solve' ms)
