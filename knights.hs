import Data.Graph.Inductive

from_ind r c i = ((i-1) `div` c, (i-1) `mod` c)
to_ind r c (ri, ci) = ri*c + ci + 1

in_bound r c (ri, ci) = ri >= 0 && ci >= 0 && ri < r && ci < c

find_jumps r c i = let (ri, ci) = from_ind r c i
                   in map (to_ind r c)
                          (filter (in_bound r c)
                                  [(ri-2, ci-1), (ri-2, ci+1),
                                   (ri-1, ci-2), (ri-1, ci+2),
                                   (ri+1, ci-2), (ri+1, ci+2),
                                   (ri+2, ci-1), (ri+2, ci+1)])

get_edges r c = concat [map (\ri -> (i, ri))
                            (find_jumps r c i) | i <- [1..r*c]]

mk_board :: Int -> Int -> (Gr () ())
mk_board r c = mkGraph (zip [1 .. r*c] (repeat ()))
                       (map (\(i, j) -> (i, j, ()))
                            (get_edges r c))

next_hop g ns = foldr (get_min g) Nothing ns

get_min g n (Just (d, ns)) | outdeg g n == d = Just (d, n:ns)
                           | outdeg g n < d = Just (outdeg g n, [n])
                           | otherwise = Just (d, ns)
get_min g n Nothing = Just (outdeg g n, [n])

