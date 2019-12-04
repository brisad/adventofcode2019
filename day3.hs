{-# OPTIONS_GHC -Wall #-}

import Data.Char

data Direction = L | U | R | D deriving (Show, Read)
data Movement = M Direction Int
type Pos = (Int, Int)
data Segment = Horiz Int Int Int | Vert Int Int Int
  deriving Show

instance Show Movement where
  show (M dir amount) = show dir ++ show amount

instance Read Movement where
  readsPrec _ (d:xs) =
      let dir = read [d]
          (num, rest) = span isDigit xs
          amount = read num
          in
          [(M dir amount, rest)]
  readsPrec _ _ = []

stepsTo :: Pos -> [Segment] -> Int
stepsTo _ []     = 0
stepsTo p (s:ss) = case lenTo p s of
    Just n  -> n
    Nothing -> seglen s + stepsTo p ss

within :: Int -> Int -> Int -> Bool
within x a b = min a b <= x && max a b >= x

lenTo :: Pos -> Segment -> Maybe Int
lenTo (px, py) (Horiz y x1 x2)
  | y == py && within px x1 x2 = Just (abs (x1 - px))
  | otherwise                  = Nothing
lenTo (px, py) (Vert x y1 y2)
  | x == px && within py y1 y2 = Just (abs (y1 - py))
  | otherwise                  = Nothing

seglen :: Segment -> Int
seglen (Horiz _ x1 x2) = abs (x2 - x1)
seglen (Vert  _ y1 y2) = abs (y2 - y1)

origin :: Pos
origin = (0, 0)

end :: Segment -> Pos
end (Horiz y _ x) = (x, y)
end (Vert  x _ y) = (x, y)

segments :: [Movement] -> [Segment]
segments = snd . foldl accumulate (origin, [])
    where accumulate (start, ss) m = let s      = segment m start
                                         start' = end s
                                     in (start', ss ++ [s])
          segment :: Movement -> Pos -> Segment
          segment (M L amount) (sx, sy) = Horiz sy sx (sx-amount)
          segment (M R amount) (sx, sy) = Horiz sy sx (sx+amount)
          segment (M U amount) (sx, sy) = Vert  sx sy (sy+amount)
          segment (M D amount) (sx, sy) = Vert  sx sy (sy-amount)

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

intersect :: Segment -> Segment -> [Pos]
intersect (Horiz y x1 x2) (Vert x y1 y2)
  | within x x1 x2 && within y y1 y2 = [(x, y)]
  | otherwise                        = []
intersect s1@(Vert _ _ _) s2@(Horiz _ _ _) = intersect s2 s1
intersect (Horiz y1 x1 x2) (Horiz y2 x3 x4)
  | y1 == y2  = [(x, y1) | x <- [max x1 x3..min x2 x4]]
  | otherwise = []
intersect (Vert x1 y1 y2) (Vert x2 y3 y4)
  | x1 == x2  = [(x1, y) | y <- [max y1 y3..min y2 y4]]
  | otherwise = []

intersections :: [Segment] -> [Segment] -> [Pos]
intersections xs ys = filter (/= origin) (concat [intersect x y | x <- xs, y <- ys])

solve :: [Movement] -> [Movement] -> Int
solve xs ys = minimum [manhattan origin z | z <- intersections xss yss]
    where xss = segments xs
          yss = segments ys

solve' :: [Movement] -> [Movement] -> Int
solve' xs ys = minimum [stepsTo z xss + stepsTo z yss | z <- intersections xss yss]
    where xss = segments xs
          yss = segments ys

main :: IO ()
main = do
    x <- getLine
    y <- getLine
    let xs = read $ "[" ++ x ++ "]"
    let ys = read $ "[" ++ y ++ "]"
    putStrLn $ "Part One: " ++ show (solve  xs ys)
    putStrLn $ "Part Two: " ++ show (solve' xs ys)
