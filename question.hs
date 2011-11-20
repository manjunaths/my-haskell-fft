{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Vector.Unboxed as D
import Data.Vector as B

-- 0 1 2 3
-- 4 5 6 7
-- 8 9 10 11

xs = [0,1,2,3,4,5,6,7,8,9,10,11]
rows = 3
cols = 4

sb = D.fromList xs

takeRows::Int -> Int -> D.Vector Int -> D.Vector Int
takeRows rows cols x0 = D.concatMap getRow starts
                        where
                          starts = D.enumFromStepN 0 cols rows
                          getRow x = D.slice x (fromIntegral cols) x0

-- takeCols::Int -> Int -> D.Vector Int -> D.Vector Int
-- takeRows rows cols x0 = D.concatMap getCol starts
--                         where
--                           starts = DV.concatMap (\x -> DV.map ((!) b) (enumFromStepN x cols rows)) $ (enumFromStepN 0 1 cols)
--                           getCol x = D.map (\b -> (!) b x0) 

-- takeRowsList::Int -> Int -> Vector Int -> [Vector Int]
-- takeRowsList rows cols x0 = Prelude.map (\x -> D.slice x (fromIntegral cols) x0) starts
--                         where
--                           starts = D.toList . D.enumFromStepN 0 cols $ rows
