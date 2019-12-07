{-# OPTIONS_GHC -Wall #-}

import System.Environment

type Pos = Int
type Prog = [Int]
type State = (Pos, Prog)

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

opcode :: State -> Int
opcode (n, p) = (p !! n) `mod` 100

modes :: Int -> [Mode]
modes x = f (x `div` 100)
    where f n = case n `mod` 10 of
                    0 -> Pos : f (n `div` 10)
                    1 -> Imm : f (n `div` 10)
                    _ -> error $ "Invalid parameter mode: " ++ show n

params :: State -> [Param]
params (n, p) = zipWith Param ms xs
    where ms = modes (p !! n)
          xs = drop (n + 1) p

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

mut :: Prog -> Pos -> Int -> Prog
mut p n v = take n p ++ [v] ++ drop (n+1) p

eval :: Prog -> Param -> Int
eval p (Param Pos x) = p !! x
eval _ (Param Imm x) = x

exec :: State -> IO ()
exec s@(n, p) = case decode s of
    Add p1 p2 (Param Pos t) -> return () >>
                               exec (n + 4, mut p t (eval p p1 + eval p p2))
    Mul p1 p2 (Param Pos t) -> return () >>
                               exec (n + 4, mut p t (eval p p1 * eval p p2))
    Inp (Param Pos t)       -> getLine >>=
                               \x -> exec (n + 2, mut p t (read x))
    Out p1                  -> putStrLn (show (eval p p1)) >>
                               exec (n + 2, p)
    Jt  p1 p2               -> return () >>
                               exec (if (eval p p1) /= 0 then eval p p2 else n + 3, p)
    Jf  p1 p2               -> return () >>
                               exec (if (eval p p1) == 0 then eval p p2 else n + 3, p)
    Lt  p1 p2 (Param Pos t) -> return () >>
                               exec (n + 4, mut p t (if (eval p p1) < (eval p p2) then 1 else 0))
    Eq  p1 p2 (Param Pos t) -> return () >>
                               exec (n + 4, mut p t (if (eval p p1) == (eval p p2) then 1 else 0))
    Hlt                     -> return ()
    instr                   -> error $ "Invalid instruction: " ++ show instr

solve :: Prog -> IO ()
solve p = exec (0, p)

main :: IO ()
main = do
    args <- getArgs
    let file = head args
    c <- readFile file
    let prog = read $ "[" ++ c ++ "]"
    solve prog
