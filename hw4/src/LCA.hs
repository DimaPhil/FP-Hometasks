module LCA where

data InNode a = InNode { label :: a, parent :: Maybe (InNode a) }

leastCommonAncestor :: Eq a => InNode a -> InNode a -> Maybe (InNode a)
leastCommonAncestor v1 v2 = lastCommon $ zip (reverse l1) (reverse l2) where
    goUp :: InNode a -> [InNode a]
    goUp InNode{parent = Nothing}                  = []
    goUp node@InNode{label = l, parent = Just par} = node : goUp par
    l1 = goUp v1
    l2 = goUp v2
    lastCommon other = (if null xs then Nothing else Just xs) >>= (Just . fst . last) where
        xs = filter (\(xs, ys) -> label xs == label ys) other
