module Model.Model where

type StackOp = [Int] -> [Int]

push :: Int -> StackOp
push n = \stk -> n : stk

binaryOp :: (Int -> Int -> Int) -> StackOp
binaryOp _ [] = error "empty list"
binaryOp _ [x] = error "not enough items"
binaryOp f (a:b:xs) = a `f` b : xs

add :: StackOp
add = binaryOp (+)

mul :: StackOp
mul = binaryOp (*)

sub :: StackOp
sub = binaryOp (-)

eq :: StackOp
eq = binaryOp (\a b -> if a==b then 1 else 0)

gt :: StackOp
gt = binaryOp (\a b -> if a>b then 1 else 0)

lt :: StackOp
lt = binaryOp (\a b -> if a<b then 1 else 0)

dup :: StackOp
dup [] = error "empty list"
dup [x] = [x,x]
dup (x:xs) = x : x : xs

dot :: StackOp
dot [] = error "empty list"
dot (x:xs) = xs

run :: [Int] -> [StackOp] -> [Int]
run stk [] = stk
run stk [op] = op stk
run stk (op: ops) = run (op stk) ops

parse :: [String] -> [StackOp]
parse [] = []
parse ("*": xs) = mul : parse xs
parse ("-": xs) = sub : parse xs
parse ("+": xs) = add : parse xs
parse ("<": xs) = lt : parse xs
parse (">": xs) = gt : parse xs
parse ("=": xs) = eq : parse xs
parse ("dup": xs) = dup : parse xs
parse (x: xs) = push (read x :: Int) : parse xs

runS :: String -> [Int]
runS s = run [] $ parse $ words s
