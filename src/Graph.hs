module Graph (Graph, fromFunction, toList, vertices) where


import qualified Data.Map as M


type Graph vertex edge = M.Map vertex [(edge, vertex)]


fromFunction :: (Ord vertex) => (vertex -> edge -> vertex) -> vertex -> [edge] -> Graph vertex edge
fromFunction func start labels = go start M.empty
  where go vertex graph
          | M.member vertex graph = graph
          | otherwise = foldr go newGraph edges
            where edges = map (func vertex) labels
                  newGraph = M.insert vertex (zip labels edges) graph

toList :: Graph vertex edge -> [(vertex, edge, vertex)]
toList = concatMap (\(x, tuples) -> map (consTuple x) tuples) . M.toList
  where consTuple x (y, z) = (x, y, z)

vertices :: Graph vertex edge -> [vertex]
vertices = M.keys

