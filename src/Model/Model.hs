module Model.Model where

type StackOp = [Int] -> [Int]

push :: Int -> StackOp
push n = \stk -> n : stk

add :: StackOp
add [] = error "empty list"
add [x] = error "not enough items"
add (a:b:xs) = a+b : xs

mul :: StackOp
mul [] = error "empty list"
mul [x] = error "not enough items"
mul (a:b:xs) = a*b : xs

sub :: StackOp
sub [] = error "empty list"
sub [x] = error "not enough items"
sub (a:b:xs) = a-b : xs

eq :: StackOp
eq [] = error "empty list"
eq [x] = error "not enough items"
eq (a:b:xs) = iseq : xs where
    iseq = if a==b then 1 else 0

gt :: StackOp
gt [] = error "empty list"
gt [x] = error "not enough items"
gt (a:b:xs) = isgt : xs where
    isgt = if a>b then 1 else 0

lt :: StackOp
lt [] = error "empty list"
lt [x] = error "not enough items"
lt (a:b:xs) = islt : xs where
    islt = if a<b then 1 else 0

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
