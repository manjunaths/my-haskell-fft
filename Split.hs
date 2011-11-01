module Split (splitEvery) where

import Data.Vector

splitEvery n xs | Data.Vector.null xs = Data.Vector.empty
                | otherwise = Data.Vector.cons (Data.Vector.take n xs)  (splitEvery n (Data.Vector.drop n xs))


-- splitEvery n xs = splitEvery' n xs Data.Vector.empty
--                   where
--                     splitEvery' n xs ml | Data.Vector.null xs = ml
--                                         | otherwise = splitEvery' n (Data.Vector.drop n xs) (ml Data.Vector.++ (Data.Vector.singleton (Data.Vector.take n xs)))

