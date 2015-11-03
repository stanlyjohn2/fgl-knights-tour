import Data.Graph.Inductive
import System.Random

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

get_min g n (Just (d, ns)) | outdeg g n == d = Just (d, n:ns)
                           | outdeg g n < d = Just (outdeg g n, [n])
                           | otherwise = Just (d, ns)
get_min g n Nothing = Just (outdeg g n, [n])

select_hop g ns = foldr (get_min g) Nothing ns

next_hop g n rgen = let (c, g') = match n g
                        Just (_, _, _, out_gns) = c
                        out_ns = (map (\(_, n') -> n') out_gns)
                    in case select_hop g' out_ns of
                        Just (_, min_hops) -> let (idx, rgen') = randomR (0, length min_hops - 1) rgen
                                              in (g', Just (min_hops !! idx), rgen')
                        Nothing -> (g', Nothing, rgen)

hop_seq g n rgen = case next_hop g n rgen of
                   (g', Just n', rgen') -> let (path, found, rgen'') = hop_seq g' n' rgen'
                                           in (n:path, found, rgen'')
                   (g', Nothing, rgen') -> ([n], isEmpty g', rgen')

iter_find g 1 rgen = hop_seq g 1 rgen
iter_find g tries rgen = let (path, found, rgen') = hop_seq g 1 rgen
                         in if found then (path, found, rgen') else iter_find g (tries - 1) rgen'
