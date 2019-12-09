{-# OPTIONS_GHC -Wall #-}

import Data.Maybe

type Pos = Int
type Prog = [Int]
data State = State { pos :: Pos, prog :: Prog, input :: Maybe Int, output :: Maybe Int }
data StopReason = Halt | Input | Output

data Mode = Pos | Imm
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
           | Hlt
    deriving Show

rd :: State -> Int -> Int
rd s n = prog s !! n

wr :: Pos -> Int -> State -> State
wr n v s = s { prog=take n (prog s) ++ [v] ++ drop (n+1) (prog s) }

opcode :: State -> Int
opcode s = rd s (pos s) `mod` 100

modes :: Int -> [Mode]
modes x = f (x `div` 100)
    where f n = case n `mod` 10 of
                    0 -> Pos : f (n `div` 10)
                    1 -> Imm : f (n `div` 10)
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
               99 -> Hlt
               _ -> error $ "Invalid opcode: " ++ show op

eval :: State -> Param -> Int
eval s (Param Pos x) = rd s x
eval _ (Param Imm x) = x

advance :: Int -> State -> State
advance n s = s { pos=pos s + n }

getOutput :: State -> Int
getOutput s = fromJust (output s)

exec :: State -> (StopReason, State)
exec s = case decode s of
    Add p1 p2 (Param Pos t) -> exec . advance 4 . wr t (eval s p1 + eval s p2) $ s
    Mul p1 p2 (Param Pos t) -> exec . advance 4 . wr t (eval s p1 * eval s p2) $ s
    Inp (Param Pos t)       -> case input s of
                                   Just x  -> exec . advance 2 . wr t x $ s { input=Nothing }
                                   Nothing -> (Input, s)
    Out p1                  -> (Output, advance 2 $ s { output=Just (eval s p1) })
    Jt  p1 p2               -> exec s { pos=if (eval s p1) /= 0 then eval s p2 else pos s + 3 }
    Jf  p1 p2               -> exec s { pos=if (eval s p1) == 0 then eval s p2 else pos s + 3 }
    Lt  p1 p2 (Param Pos t) -> exec . advance 4 . wr t (if (eval s p1) <  (eval s p2) then 1 else 0) $ s
    Eq  p1 p2 (Param Pos t) -> exec . advance 4 . wr t (if (eval s p1) == (eval s p2) then 1 else 0) $ s
    Hlt                     -> (Halt, s)
    instr                   -> error $ "Invalid instruction: " ++ show instr

intersperse :: a -> [a] -> [[a]]
intersperse x []     = [[x]]
intersperse x (y:ys) = [x:y:ys] ++ map (y:) (intersperse x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (intersperse x) (perms xs))

setupAmp :: Prog -> Int -> State
setupAmp p phase = snd (exec State { pos=0 , prog=p , input=Just phase , output=Nothing })

runCascade :: Prog -> [Int] -> Int
runCascade p phases = f amps 0
    where amps = [setupAmp p phase | phase <- phases]
          f (amp:rest) nextInp = case exec amp of
                (Halt, _)      -> nextInp
                (Input, amp')  -> f (amp' { input=Just nextInp } : rest) nextInp
                (Output, amp') -> f (rest ++ [amp']) (getOutput amp')
          f [] _               = error "Nothing to run on"

solve :: Prog -> Int
solve p =  maximum [runCascade p phases | phases <- perms [0..4]]

solve' :: Prog -> Int
solve' p = maximum [runCascade p phases | phases <- perms [5..9]]

main :: IO ()
main = do
    c <- getContents
    let p = read $ "[" ++ c ++ "]"
    putStrLn $ "Part One: " ++ show (solve  p)
    putStrLn $ "Part Two: " ++ show (solve' p)
