{-# OPTIONS_GHC -Wall #-}

rmcommon :: Eq a => [a] -> [a] -> ([a], [a])
rmcommon [] ys = ([], ys)
rmcommon xs [] = (xs, [])
rmcommon (x:xs) (y:ys) | x == y    = rmcommon xs ys
                       | otherwise = ((x:xs), (y:ys))

path :: String -> [(String, String)] -> [String]
path k t = case [v | (v, k') <- t, k == k'] of
    []  -> []
    x:_ -> x : path x t

splitobj :: String -> (String, String)
splitobj s = let (x, y) = break (== ')') s in (x, drop 1 y)

solve :: [(String, String)] -> Int
solve t = length (concat [path v t | (_, v) <- t])

solve' :: [(String, String)] -> Int
solve' t = length ((uncurry (++)) (rmcommon (reverse you) (reverse san)))
    where you = path "YOU" t
          san = path "SAN" t

main :: IO ()
main = do
    c <- getContents
    let t = map splitobj (words c)
    putStrLn $ "Part One: " ++ show (solve  t)
    putStrLn $ "Part Two: " ++ show (solve' t)
