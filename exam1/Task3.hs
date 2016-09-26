module Third where

--We need to write data structure - stack with current minimum in it, supporting operations `pop`, `push`, `multipop`

data Stack a = Empty | Node (a, a) (Stack a) deriving Show

push :: Ord a => Stack a -> a -> Stack a
push Empty x = Node (x, x) Empty
push node@(Node (v, minv) _) x = Node (x, min x minv) node

pop :: Stack a -> Stack a
pop Empty = error "Can't pop from empty stack"
pop (Node _ stack) = stack

multipop :: Stack a -> Int -> Stack a
multipop stack 0 = stack
multipop stack k = multipop (pop stack) (k - 1)

getMin :: Stack a -> a
getMin Empty = error "There is no minimum value in empty stack"
getMin (Node (_, minv) _) = minv

stack = push (push (push (push (push (push Empty 3) 2) 4) 5) 1) 7
--getMin stack == 1
--getMin (pop stack) == 1
--getMin (pop (pop stack)) == 2
--getMin (multipop stack 2) == 2
--getMin (multipop stack 5) == 3
--getMin (multipop stack 6) == exception
--multipop stack 7 == exception