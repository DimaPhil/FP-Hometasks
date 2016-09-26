module Fourth where

--We need to write data structure - stack with current minimum in it, supporting operations `pop`, `push`, `multipop`
--Here we need to implement instance for Show class

data Stack a = Empty | Node (a, a) (Stack a)

showElement :: Show a => (a, a) -> String
showElement (x, minv) = "(value = " ++ show x ++ ", min_value = " ++ show minv ++ ")"
  
instance Show a => Show (Stack a) where
  show Empty = "[]"
  show (Node element Empty) = showElement element
  show (Node element stack) = show stack ++ ":" ++ showElement element

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
--show stack == "(3,3):(2,2):(4,2),(5,2),(1,1),(7,1)"
--show (pop stack) == "(3,3):(2,2):(4,2),(5,2),(1,1)"
--show (pop (pop stack)) == "(3,3):(2,2):(4,2),(5,2)"
--show (multipop stack 2) == "(3,3):(2,2):(4,2),(5,2)"
--show (multipop stack 5) == "(3,3)"
--show (multipop stack 6) == "[]"
--show (multipop stack 7) == exception