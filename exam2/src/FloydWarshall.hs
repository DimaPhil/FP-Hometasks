module FloydWarshall where

import           Control.Monad.Trans.Maybe (MaybeT ())
import           Data.Char                 (isAlpha)
import           Data.List                 (nub)
import           Text.Parsec
import           Text.Parsec.String
import           Text.Printf               (printf)

type Vertex a = a
type Edge a = (a, a)

data Graph a = Graph { vertices :: [Vertex a]
                     , edges    :: [Edge a] }

instance (Show a) => Show (Graph a) where
    show g = printf "Vertices: %s\nEdges:\n%s"
                    (unwords . map show $ vertices g)
                    (unlines . map showEdge $ edges g)
        where
          showEdge (x, y) = printf "%s -> %s" (show x) (show y)

edge :: Parser (Edge String)
edge = do
  u <- many1 alphaNum
  spaces >> string "->" >> spaces
  v <- many1 alphaNum
  spaces
  return (u, v)

readGraph :: IO (Graph String)
readGraph = undefined

floyd :: (Eq a) => Graph a -> Graph a
floyd g = Graph { edges = go (vertices g) (edges g)
                   , vertices = vertices g }
    where
      go vs es = foldl (\es v -> nub (es ++ [(a, d) | (a, b) <- es, (c, d) <- es, b == c])) es vs

main :: IO ()
main = print =<< fmap floyd readGraph
