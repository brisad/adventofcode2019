{-# OPTIONS_GHC -Wall #-}

import Data.Char
import Text.ParserCombinators.ReadP

data Vec = Vec Int Int Int
data Body = Body { pos :: Vec, vel :: Vec }

instance Show Vec where
  show (Vec x y z) =
    "<x=" ++ show x ++ ", y=" ++ show y ++ ", z=" ++ show z ++ ">"

instance Read Vec where
  readsPrec _ = readP_to_S $
    char '<' *> (Vec <$> (component 'x') <*> (component 'y') <*> (component 'z')) <* char '>'
    where component c = char c *> char '=' *> number <* optional (string ", ")
          number = do
              sign <- option 1 (const (-1) <$> char '-')
              (* sign) . read <$> many1 (satisfy isDigit)

dv :: Vec -> Vec -> Vec
dv (Vec ux uy uz) (Vec vx vy vz) =
    Vec (signum (vx - ux)) (signum (vy - uy)) (signum (vz - uz))

addVec :: Vec -> Vec -> Vec
addVec (Vec ux uy uz) (Vec vx vy vz) = Vec (ux + vx) (uy + vy) (uz + vz)

gravity :: Body -> [Body] -> Body
gravity b bodies = foldl f b bodies
    where f body other = body { vel=addVec (vel body) (dv (pos body) (pos other)) }

velocity :: Body -> Body
velocity body = body { pos=addVec (pos body) (vel body) }

pot :: Body -> Int
pot body = abs x + abs y + abs z
    where (Vec x y z) = pos body

kin :: Body -> Int
kin body = abs x + abs y + abs z
    where (Vec x y z) = vel body

tot :: Body -> Int
tot body = pot body * kin body

step :: [Body] -> [Body]
step bodies = map velocity bodies'
    where bodies' = [gravity b bodies | b <- bodies]

period :: Eq a => [a] -> Int
period (x:rest) = f 1 rest
    where f n (y:ys) | x == y    = n
                     | otherwise = f (n+1) ys
          f _ []                 = error "Unexpected end"
period []       = 0

solve :: [Body] -> Int
solve bodies = sum $ map tot final
    where steps = iterate step bodies
          final = head (drop 1000 steps)

solve' :: [Body] -> Int
solve' bodies = foldr1 lcm (fmap period [xs, ys, zs])
    where steps = iterate step bodies
          xs = map (map onlyX) steps
          ys = map (map onlyY) steps
          zs = map (map onlyZ) steps
          onlyX (Body (Vec p _ _) (Vec v _ _)) = (p, v)
          onlyY (Body (Vec _ p _) (Vec _ v _)) = (p, v)
          onlyZ (Body (Vec _ _ p) (Vec _ _ v)) = (p, v)

main :: IO ()
main = do
    c <- getContents
    let bodies = [Body p (Vec 0 0 0) | p <- map read (lines c)]
    putStrLn $ "Part One: " ++ show (solve  bodies)
    putStrLn $ "Part Two: " ++ show (solve' bodies)
