{-# LANGUAGE BangPatterns #-}
 
import qualified Data.ByteString.Lazy.Char8      as BS
import qualified Data.ByteString.Lex.Lazy.Double as BS
import qualified Data.Vector.Unboxed as DV
import Prelude hiding ((++), length, map, sum, zipWith3)
import Data.Vector.Unboxed (Vector, (++), (!), length, map, imap, sum, zipWith3, enumFromStepN)
import qualified Data.Vector.Algorithms.Intro as DV
import Data.Complex
import System.Environment
import Control.Monad
  
headerSize bs = (+) 4 (BS.length . BS.concat . Prelude.take 4 . BS.lines $ bs)  
  
dropHeader bs = BS.drop (headerSize bs) $ bs

main = do
    [f] <- getArgs
    s   <- BS.readFile f
    putStrLn "P2"
    putStrLn "# Created by haskell-dft"
    putStrLn "512 512"
    putStrLn "255"
    -- DV.mapM print . map ((round::RealFrac a => a -> Int) . magnitude) . (fft2D 512 512) . DV.map (\x -> (x:+0)) . parse $ (dropHeader s)
    DV.mapM print . scale . map (magnitude) . (fft2D 512 512) . DV.map (\x -> (x:+0)) . parse $ (dropHeader s)
    
scale::Vector Double -> Vector Double
scale xs = map (\x -> (x / maxVec)*255.0) xs
  where maxVec = DV.sort =<< return xs
        

colMajor::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
colMajor rows cols b = DV.concatMap (\x -> DV.map ((!) b) (enumFromStepN x cols rows)) $ (enumFromStepN 0 1 cols)
--colMajor b cols rows = DV.map (\x -> DV.map (b!) (enumFromStepN x cols rows)) (enumFromStepN 0 1 cols)

fft2D::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
fft2D rows cols xs = fft_CT . colMajor rows cols . fft_CT $ xs

dft::Vector (Complex Double) -> Vector (Complex Double)
dft xs = map (\k -> sum (imap (arg k) xs)) ((numvec n)::Vector (Int))
  where
    n = length xs
    nf = fromIntegral n
    numvec n = enumFromStepN 0 1 n
    arg k i x = x * exp (-2 * pi * kf * ifl * (0:+1)/nf)
              where
                kf = fromIntegral k
                ifl = fromIntegral i
      
idft::Vector (Complex Double) -> Vector (Complex Double)
idft xs = map (\k ->(/) (sum (imap (arg k) xs)) nf) ((numvec n)::Vector (Int))
  where
    n = length xs
    nf = fromIntegral n
    numvec n = enumFromStepN 0 1 n
    arg k i x = x * exp (2 * pi * kf * ifl * (0:+1)/nf)
              where
                kf = fromIntegral k
                ifl = fromIntegral i

fft_CT xs = if (length xs) == 1 then
              dft xs
            else
              let xtop = zipWith3 fft_top fxse fxso (enumFromStepN 0 1 m)
              
                  xbottom = zipWith3 fft_bottom fxse fxso (enumFromStepN 0 1 m)
                  xse = map ((!) xs) (enumFromStepN 0 2 m)
                  xso = map ((!) xs) (enumFromStepN 1 2 m)
                  fxse = fft_CT xse
                  fxso = fft_CT xso
                  n = length $ xs
                  nf = fromIntegral n
                  m = n `div` 2
                  fft_top xe xo k = xe + xo * exp (-2 * (0:+1) * pi * k / nf)
                  fft_bottom xe xo k = xe - xo * exp (-2 * (0:+1) * pi * k / nf)
              in              
                  (++) xtop xbottom

ifft_CT xs = if (length xs) == 1 then
               idft xs
            else
               let xtop = zipWith3 fft_top fxse fxso (enumFromStepN 0 1 m)
                   xtop_scaled = map (/2) xtop
                   xbottom = zipWith3 fft_bottom fxse fxso (enumFromStepN 0 1 m)
                   xbottom_scaled = map (/2) xbottom
                   xse = map ((!) xs) (enumFromStepN 0 2 m)
                   xso = map ((!) xs) (enumFromStepN 1 2 m)
                   fxse = ifft_CT xse
                   fxso = ifft_CT xso
                   n = length $ xs
                   nf = fromIntegral n
                   m = n `div` 2
                   fft_top xe xo k = xe + xo * exp (2 * (0:+1) * pi * k / nf)
                   fft_bottom xe xo k = xe - xo * exp (2 * (0:+1) * pi * k / nf)
              in              
                (++) xtop_scaled xbottom_scaled


-- Fill a new vector from a file containing a list of numbers.
parse = DV.unfoldr step
  where
    step !s = case BS.readDouble s of
      Nothing       -> Nothing
      Just (!k, !t) -> Just (k, BS.tail t)
      