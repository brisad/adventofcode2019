{-# OPTIONS_GHC -Wall #-}

increasing :: String -> Bool
increasing []       = True
increasing [_]      = True
increasing (x:y:xs) = x <= y && increasing (y:xs)

count :: Eq a => a -> [a] -> Int
count t xs = length [x | x <- xs, x == t]

digitCounts :: String -> [Int]
digitCounts s = [count d s | d <- ['0'..'9']]

solve :: Int -> Int -> Int
solve lo hi =  length . filter (any (>= 2) . digitCounts) . filter increasing .
               map show $ [lo..hi]

solve' :: Int -> Int -> Int
solve' lo hi = length . filter (any (== 2) . digitCounts) . filter increasing .
               map show $ [lo..hi]

main :: IO ()
main = do
    putStrLn $ "Part One: " ++ show (solve  240298 784956)
    putStrLn $ "Part Two: " ++ show (solve' 240298 784956)
