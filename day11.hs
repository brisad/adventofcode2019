{-# OPTIONS_GHC -Wall #-}

import Data.Maybe

type Pos = Int
type Prog = [Int]
data State = State { pos :: Pos
                   , prog :: Prog
                   , input :: Maybe Int
                   , output :: Maybe Int
                   , base :: Int
                   }
    deriving Show
data StopReason = Halt | Input | Output

data Mode = Pos | Imm | Rel
    deriving Show
data Param = Param Mode Int
    deriving Show
data Instr = Add Param Param Param
           | Mul Param Param Param
           | Inp Param
           | Out Param
           | Jt  Param Param
           | Jf  Param Param
           | Lt  Param Param Param
           | Eq  Param Param Param
           | Adj Param
           | Hlt
    deriving Show

rd :: State -> Int -> Int
rd s n | n < length (prog s) = prog s !! n
       | otherwise           = 0

wr :: Pos -> Int -> State -> State
wr n v s | n < length (prog s) = s { prog=take n (prog s) ++ [v] ++ drop (n+1) (prog s) }
         | otherwise           = s { prog=(prog s) ++ take m (repeat 0) ++ [v] }
                                 where m = n - length (prog s)

wrp :: Param -> Int -> State -> State
wrp (Param Pos x) v s = wr x v s
wrp (Param Rel x) v s = wr (base s + x) v s
wrp (Param Imm _) _ _ = error "Cannot write with immediate mode"

opcode :: State -> Int
opcode s = rd s (pos s) `mod` 100

modes :: Int -> [Mode]
modes x = f (x `div` 100)
    where f n = case n `mod` 10 of
                    0 -> Pos : f (n `div` 10)
                    1 -> Imm : f (n `div` 10)
                    2 -> Rel : f (n `div` 10)
                    _ -> error $ "Invalid parameter mode: " ++ show n

params :: State -> [Param]
params s = zipWith Param ms xs
    where ms = modes (rd s (pos s))
          xs = (drop (pos s + 1) (prog s)) ++ repeat 0

decode :: State -> Instr
decode s = let op      = opcode s
               x:y:z:_ = params s
           in
           case op of
               1  -> Add x y z
               2  -> Mul x y z
               3  -> Inp x
               4  -> Out x
               5  -> Jt  x y
               6  -> Jf  x y
               7  -> Lt  x y z
               8  -> Eq  x y z
               9  -> Adj x
               99 -> Hlt
               _ -> error $ "Invalid opcode: " ++ show op

eval :: State -> Param -> Int
eval s (Param Pos x) = rd s x
eval _ (Param Imm x) = x
eval s (Param Rel x) = rd s (base s + x)

advance :: Int -> State -> State
advance n s = s { pos=pos s + n }

getOutput :: State -> Int
getOutput s = fromJust (output s)

exec :: State -> (StopReason, State)
exec s = case decode s of
    Add p1 p2 p3 -> exec . advance 4 . wrp p3 (eval s p1 + eval s p2) $ s
    Mul p1 p2 p3 -> exec . advance 4 . wrp p3 (eval s p1 * eval s p2) $ s
    Inp p1       -> case input s of
                      Just x  -> exec . advance 2 . wrp p1 x $ s { input=Nothing }
                      Nothing -> (Input, s)
    Out p1       -> (Output, advance 2 $ s { output=Just (eval s p1) })
    Jt  p1 p2    -> exec s { pos=if (eval s p1) /= 0 then eval s p2 else pos s + 3 }
    Jf  p1 p2    -> exec s { pos=if (eval s p1) == 0 then eval s p2 else pos s + 3 }
    Lt  p1 p2 p3 -> exec . advance 4 . wrp p3 (if (eval s p1) <  (eval s p2) then 1 else 0) $ s
    Eq  p1 p2 p3 -> exec . advance 4 . wrp p3 (if (eval s p1) == (eval s p2) then 1 else 0) $ s
    Adj p1       -> exec . advance 2 $ s { base=base s + eval s p1 }
    Hlt          -> (Halt, s)

type Assoc k v = [(k, v)]
data Direction = L | U | R | D deriving (Enum, Show)
data Turn = TL | TR deriving Show
data Color = Black | White deriving Show
data Robot = Robot { loc :: (Int, Int)
                   , dir :: Direction
                   , painted :: Assoc (Int, Int) Color
                   , counter :: Int
                   } deriving Show

showDrawing :: Robot -> String
showDrawing r = row (ymin - 1)
    where panels = painted r
          row y | y == (ymax + 2) = ""
          row y | otherwise = "\n"
                              ++ [showColor (lookup (x, y) panels) | x <- [xmin..xmax]]
                              ++ row (y + 1)
          showColor Nothing      = '█'
          showColor (Just Black) = '█'
          showColor (Just White) = ' '
          coords = map fst panels
          xcoords = map fst coords
          ycoords = map snd coords
          (xmin, xmax) = (minimum xcoords, maximum xcoords)
          (ymin, ymax) = (minimum ycoords, maximum ycoords)

insert :: Eq k => k -> v -> Assoc k v -> Assoc k v
insert k v []            = [(k, v)]
insert k v ((k', v'):xs)
    | k' == k            = (k, v) : xs
    | otherwise          = (k', v') : insert k v xs

contains :: Eq k => k -> Assoc k v -> Bool
contains k t = length [v | (k', v) <- t, k' == k] > 0

isWhite :: Robot -> Bool
isWhite r = case lookup (loc r) (painted r) of
    Just White -> True
    _          -> False

paint :: Color -> Robot -> Robot
paint c r = r { painted=insert (loc r) c (painted r)
              , counter=if contains (loc r) (painted r) then
                           (counter r) else (counter r) + 1
              }

turn :: Turn -> Robot -> Robot
turn TL r = r { dir=toEnum ((fromEnum (dir r) - 1) `mod` 4) }
turn TR r = r { dir=toEnum ((fromEnum (dir r) + 1) `mod` 4) }

forward :: Robot -> Robot
forward r = case (dir r) of
              L -> r { loc=left  (loc r) }
              U -> r { loc=up    (loc r) }
              R -> r { loc=right (loc r) }
              D -> r { loc=down  (loc r) }
    where left  (x, y) = (x - 1, y)
          up    (x, y) = (x, y - 1)
          right (x, y) = (x + 1, y)
          down  (x, y) = (x, y + 1)

run :: Prog -> Robot -> Robot
run p robot = f (State 0 p Nothing Nothing 0) robot
    where f s r = case exec s of
                      (Halt, _)     -> r
                      (Input, s')   -> f s' { input=Just (if isWhite r then 1 else 0) } r
                      (Output,  s') -> case exec s' of
                                         (Output, s'') -> f s'' (ctrlRobot color turnDir r)
                                                          where color = toColor (getOutput s')
                                                                turnDir = toTurn (getOutput s'')
                                         _             -> error "Expected one more output"
          toColor 0 = Black
          toColor 1 = White
          toColor _ = error "Invalid color"
          toTurn 0 = TL
          toTurn 1 = TR
          toTurn _ = error "Invalid turn"
          ctrlRobot c t = forward . turn t . paint c

solve :: Prog -> Int
solve p = counter (run p (Robot (0, 0) U [] 0))

solve' :: Prog -> String
solve' p = showDrawing (run p (Robot (0, 0) U [((0, 0), White)] 0))

main :: IO ()
main = do
    c <- getContents
    let p = read $ "[" ++ c ++ "]"
    putStrLn $ "Part One: " ++ show (solve p)
    putStrLn $ "Part Two: " ++ (solve' p)
