{-# OPTIONS_GHC -Wall #-}

import Data.Char
import Data.List (minimumBy)
import Data.Ord

type Dim = (Int, Int)
data Pixel = B | W | T deriving Eq
data Layer a = Layer Int Int [a]

instance Show Pixel where
  show B = "█"
  show W = " "
  show T = "▏"

instance Show a => Show (Layer a) where
  show (Layer w _ pxs) = rows pxs
    where rows [] = ""
          rows xs = "\n" ++ concat (map show (take w xs)) ++ rows (drop w xs)

instance Semigroup Pixel where
  T <> q = q
  p <> _ = p

instance Monoid Pixel where
  mempty = T

instance Monoid a => Semigroup (Layer a) where
  Layer w1 h1 pxs1 <> Layer w2 h2 pxs2 = Layer w h $ zipWith (<>) pxs1' pxs2'
    where w = max w1 w2
          h = max h1 h2
          pxs1' = grow (w1, h1) (w, h) pxs1
          pxs2' = grow (w2, h2) (w, h) pxs2

instance Monoid a => Monoid (Layer a) where
  mempty = Layer 0 0 []

grow :: Monoid a => (Int, Int) -> (Int, Int) -> [a] -> [a]
grow (_, 0) (w, h)     _   = take (w * h) (repeat mempty)
grow (w1, h1) (w2, h2) pxs = take w1 pxs
                             ++ take (w2 - w1) (repeat mempty)
                             ++ grow (w1, h1 - 1) (w2, h2 - 1) (drop w1 pxs)

toPixel :: Int -> Pixel
toPixel 0 = B
toPixel 1 = W
toPixel 2 = T
toPixel _ = error "Invalid pixel"

count :: Eq a => a -> Layer a -> Int
count x (Layer _ _ xs) = length [x | x' <- xs, x == x']

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

layers :: Dim -> [Int] -> [Layer Pixel]
layers (w, h) pxs = [Layer w h (map toPixel chunk) | chunk <- chunks (w * h) pxs]

values :: String -> [Int]
values = filter (\x -> x >= 0 && x <= 9) . map (subtract 48 . ord)

solve :: Dim -> [Int] -> Int
solve (w, h) pxs = count W layer * count T layer
    where layer = find (layers (w, h) pxs)
          find  = minimumBy . comparing . count $ B

solve' :: Dim -> [Int] -> Layer Pixel
solve' dim = mconcat . layers dim

main :: IO ()
main = do
    c <- getContents
    let pixels = values c
    putStrLn $ "Part One: " ++ show (solve  (25, 6) pixels)
    putStrLn $ "Part Two: " ++ show (solve' (25, 6) pixels)
