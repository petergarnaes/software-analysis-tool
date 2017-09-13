module Lattice where
import Data.List

class (Eq a) => Lattice a where
    initialize :: a -- How to initialize the lattice
    (<<) :: a -> a -> Bool -- Determine if left is ordered compared to right
    bound :: a -> a -> a -- Finds upper bound element of two elements in lattice
    super :: a -> [a] -- Gives a list of all elements that has a higher order than argument, including itself (reflexion). Return only one of each
    x << y = y `elem` super x
    bound x y = let res = intersect (super x) (super y) in head $ filter (\x -> and (map (\y -> (x << y)) res)) res -- gives point less than or equal to all other points in the intersected supersets of x and y. Works because of anti-symmetry, because it ensures at most one point can be less-than-equal to all other points
