{-# OPTIONS_GHC -Wall #-}

import Data.List (maximumBy)
import Data.Ord

type Vec = (Int, Int)

getAsteroids :: String -> [Vec]
getAsteroids contents = concat $ map row (zip [0..] (words contents))
    where row (y, xs) = [(x, y) | (x, c) <- zip [0..] xs, c == '#']

diff :: Vec -> Vec -> Vec
diff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

norm :: Vec -> Double
norm (x, y) = sqrt (fromIntegral (x * x) + fromIntegral (y * y))

deg :: Vec -> Double
deg (x, y) = if d < 0 then d + 2 * pi else d
    where d = pi / 2 - atan2 (-(fromIntegral y)) (fromIntegral x)

orderByDist :: Vec -> Vec -> Vec -> (Vec, Vec)
orderByDist orig a b | norm oa < norm ob = (a, b)
                     | otherwise         = (b, a)
    where oa = diff a orig
          ob = diff b orig

insertOnlySeen :: Vec -> Vec -> [Vec] -> [Vec]
insertOnlySeen _    a []      = [a]
insertOnlySeen orig a (b:bs)
  | aDeg /= bDeg = b : insertOnlySeen orig a bs
  | otherwise    = closest : bs
    where aDeg    = deg (diff a orig)
          bDeg    = deg (diff b orig)
          closest = fst (orderByDist orig a b)

insertInOrder :: Vec -> Vec -> [Vec] -> [Vec]
insertInOrder _    a []     = [a]
insertInOrder orig a (b:bs) =
    case compare aDeg bDeg of
        LT -> a : b : bs
        GT -> if lastInThisRevolution orig (b:bs) then
              b : a : bs else
              b : insertInOrder orig a bs
        EQ -> bs' ++ insertInOrder orig far rest
              where (bs', rest) = breakOnNextRev orig (near:bs)
                    (near, far) = orderByDist orig a b
    where aDeg = deg (diff a orig)
          bDeg = deg (diff b orig)

lastInThisRevolution :: Vec -> [Vec] -> Bool
lastInThisRevolution origin as = length (fst (breakOnNextRev origin as)) == 1

breakOnNextRev :: Vec -> [Vec] -> ([Vec], [Vec])
breakOnNextRev orig as = break2 f as
    where f a b = deg (diff a orig) >= deg (diff b orig) 

break2 :: (a -> a -> Bool) -> [a] -> ([a], [a])
break2 _ []       = ([], [])
break2 _ [x]      = ([x], [])
break2 p (x:y:ys)
    | p x y       = ([x], y:ys)
    | otherwise   = let (xs', ys') = break2 p (y:ys) in (x:xs', ys')

directAsteroids :: Vec -> [Vec] -> Int
directAsteroids origin vs = length asteroids
    where asteroids = foldr (insertOnlySeen origin) [] (filter (/= origin) vs)

best :: [Vec] -> (Vec, Int)
best as = maximumBy (comparing snd) [(a, directAsteroids a as) | a <- as]

laserOrder :: Vec -> [Vec] -> [Vec]
laserOrder orig = foldr (insertInOrder orig) []

solve :: [Vec] -> Int
solve = snd . best

solve' :: [Vec] -> Int
solve' as = x * 100 + y
    where station = fst (best as)
          asteroids = laserOrder station (filter (/= station) as)
          (x, y) = asteroids !! 199

main :: IO ()
main = do
    c <- getContents
    let as = getAsteroids c
    putStrLn $ "Part One: " ++ show (solve  as)
    putStrLn $ "Part Two: " ++ show (solve' as)
