import Data.Graph.Inductive
import System.Random

-- get the coords, given its cell index
from_ind r c i = ((i-1) `div` c, (i-1) `mod` c)
to_ind r c (ri, ci) = ri*c + ci + 1

-- check whether coords is inside the board
in_bound r c (ri, ci) = ri >= 0 && ci >= 0 && ri < r && ci < c

-- get the list of valid jumps for the knight given its cell index
find_jumps r c i = let (ri, ci) = from_ind r c i
                   in map (to_ind r c)
                          (filter (in_bound r c)
                                  [(ri-2, ci-1), (ri-2, ci+1),
                                   (ri-1, ci-2), (ri-1, ci+2),
                                   (ri+1, ci-2), (ri+1, ci+2),
                                   (ri+2, ci-1), (ri+2, ci+1)])

-- get the list of edges of valid knight jumps as a list of pairs
get_edges r c = concat [map (\ri -> (i, ri))
                            (find_jumps r c i) | i <- [1..r*c]]

-- make a Graph datastructure representing the board
mk_board :: Int -> Int -> (Gr (Int, Int) ())
mk_board r c = mkGraph (map (\i -> (i, from_ind r c i))
                            [1 .. r*c])
                       (map (\(i, j) -> (i, j, ()))
                            (get_edges r c))

-- For use in map.
-- return the extended third argument which includes n if n has degree d
-- else if n has smaller degree, return only n and its degree
-- else if n has bigger degree, discard n and simply return third argument
get_min g n (Just (d, ns)) | outdeg g n == d = Just (d, n:ns)
                           | outdeg g n < d = Just (outdeg g n, [n])
                           | otherwise = Just (d, ns)
get_min g n Nothing = Just (outdeg g n, [n])

-- return a list of edges which have minimum degree among the ones given
select_hop g ns = foldr (get_min g) Nothing ns

-- rng is a random number generator
-- return a random element from a list, along with the new rng
select_rand l rng = let (idx, rng') = randomR (0, length l - 1) rng
                    in (l !! idx, rng')

-- the knight currently at n hops to another node. A new graph g' which
-- has n removed will be returned, along with knight's new position.
-- If the knight was at a dead end, Nothing will be returned
next_hop g n rng = let (c, g') = match n g
                       Just (_, _, _, out_gns) = c
                       out_ns = (map snd out_gns)
                   in case select_hop g' out_ns of
                        Just (_, min_hops) -> let (n', rng') = select_rand min_hops rng
                                              in (Just n', g', rng')
                        Nothing -> (Nothing, g', rng)

-- returns a sequence of hops starting from n, and whether it was a knight's tour
hop_seq g n rng = case next_hop g n rng of
                    (Just n', g', rng') -> let (found, path, rng'') = hop_seq g' n' rng'
                                           in (found, n:path, rng'')
                    (Nothing, g', rng') -> (isEmpty g', [n], rng')

-- find a knights tour by trial and error, with at most `tries` trials
iter_find g n tries rng = let (found, path, rng') = hop_seq g n rng
                          in if found || tries == 1
                             then (found, path, tries - 1, rng')
                             else iter_find g n (tries - 1) rng'

knights_tour_ext rows cols start tries seed =
    let (found, path, tries_left, _) = iter_find (mk_board rows cols) start tries (mkStdGen seed)
    in (found, path, tries - tries_left)

knights_tour rows cols = let (found, path, n_tries) = knights_tour_ext rows cols 1 16 1
                         in if found
                            then Just (map (from_ind rows cols) path)
                            else Nothing
