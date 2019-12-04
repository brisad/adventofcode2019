{-# OPTIONS_GHC -Wall #-}

type Pos = Int
type Prog = [Int]
type State = (Pos, Prog)

mut :: Prog -> Pos -> Int -> Prog
mut p n v = take n p ++ [v] ++ drop (n+1) p

exec :: State -> State
exec (n, p) = case p !! n of
    1 -> (n + 4, mut p c (p !! a + p !! b))
    2 -> (n + 4, mut p c (p !! a * p !! b))
    _ -> error "Invalid opcode"
    where a = p !! (n + 1)
          b = p !! (n + 2)
          c = p !! (n + 3)

atEnd :: State -> Bool
atEnd (n, p) = p !! n == 99

runToEnd :: Prog -> State
runToEnd p = head . dropWhile (not . atEnd) $ states
    where states = iterate exec (exec (0, p))

eval :: Prog -> Int
eval = head . snd . runToEnd

setInputs :: Prog -> Int -> Int -> Prog
setInputs p noun verb = mut (mut p 1 noun) 2 verb

evalWithInputs :: Int -> Int -> Prog -> Int
evalWithInputs noun verb p = eval $ setInputs p noun verb

solve :: Prog -> Int
solve = evalWithInputs 12 2

solve' :: Prog -> Int
solve' p = 100 * noun + verb
    where (noun, verb) = head [(n, v) | n <- [0..99], v <- [0..99],
                                        evalWithInputs n v p == 19690720]

main :: IO ()
main = do
    c <- getContents
    let prog = read $ "[" ++ c ++ "]"
    putStrLn $ "Part One: " ++ show (solve  prog)
    putStrLn $ "Part Two: " ++ show (solve' prog)
