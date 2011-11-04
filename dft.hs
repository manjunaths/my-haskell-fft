{-# LANGUAGE BangPatterns #-}
 
import qualified Data.ByteString.Lazy.Char8      as BS
import qualified Data.ByteString.Lex.Lazy.Double as BS
import Data.Vector
import Data.Complex
import System.Environment
 
headerSize bs = (+) 4 (BS.length . BS.concat . Prelude.take 4 . BS.lines $ bs)  
  
dropHeader bs = BS.drop (headerSize bs) $ bs

-- main = do
--     [f] <- getArgs
--     s   <- BS.readFile f
--     parse $ (dropHeader s)
 
dft::Vector (Double) -> Vector (Complex Double)
dft xr = Data.Vector.map (\k -> Data.Vector.sum (Data.Vector.map (arg k) (numvec n))) (numvec n)
  where
    n = Data.Vector.length xr
    nf = fromIntegral n
    numvec n = Data.Vector.enumFromTo 0 (n-1)
    arg k i = ((xr!i):+0) * exp (-2 * pi * kf * ifl * (0:+1)/nf)
              where
                kf = fromIntegral k
                ifl = fromIntegral i
      
idft::Vector (Complex Double) -> Vector (Complex Double)
idft xs = Data.Vector.map (\k ->(/) (Data.Vector.sum (Data.Vector.map (arg k) (numvec n))) nf) (numvec n)
  where
    n = Data.Vector.length xs
    nf = fromIntegral n
    numvec n = Data.Vector.enumFromTo 0 (n-1)
    arg k i = (xs!i) * exp (2 * pi * kf * ifl * (0:+1)/nf)
              where
                kf = fromIntegral k
                ifl = fromIntegral i

-- Fill a new vector from a file containing a list of numbers.
parse = Data.Vector.unfoldr step
  where
    step !s = case BS.readDouble s of
      Nothing       -> Nothing
      Just (!k, !t) -> Just (k, BS.tail t)
