{-# LANGUAGE BangPatterns, NoMonomorphismRestriction #-}
 
import qualified Data.ByteString.Lazy.Char8      as BS
import qualified Data.ByteString.Lex.Lazy.Double as BS
import qualified Data.Vector.Unboxed as DV
import qualified Data.Vector as BV
import Prelude hiding ((++), length, sum, zipWith3)
import Data.Vector.Unboxed (Vector, (++), (!), length, imap, sum, zipWith3, enumFromStepN)
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
    DV.mapM print . scale . map (magnitude) . (ifft2D 512 512) . DV.map (\x -> (x:+0)) . parse $ (dropHeader s)
    
-- scale::Vector Double -> Vector Double
-- scale xs = map (\x -> (x / maxVec)*255.0) xs
--   where maxVec = DV.sort =<< return xs
        

rowDft::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
rowDft rows cols b = DV.concatMap (fft_CT . getRow) starts
                     where
                       starts = DV.enumFromStepN 0 cols $ rows
                       getRow x = DV.slice x (fromIntegral cols) b

colDft::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
colDft rows cols b = DV.concatMap (fft_CT . getCol) starts
                     where
                       starts = DV.enumFromStepN 0 1 cols
                       getCol x = DV.map ((DV.!) b) (DV.enumFromStepN x cols rows)

fft2D::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
fft2D rows cols xs = colDft rows cols . rowDft rows cols $ xs


rowiDft::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
rowiDft rows cols b = DV.concatMap (ifft_CT . getRow) starts
                     where
                       starts = DV.enumFromStepN 0 cols $ rows
                       getRow x = DV.slice x (fromIntegral cols) b

coliDft::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
coliDft rows cols b = DV.concatMap (ifft_CT . getCol) starts
                     where
                       starts = DV.enumFromStepN 0 1 cols
                       getCol x = DV.map ((DV.!) b) (DV.enumFromStepN x cols rows)

ifft2D::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
ifft2D rows cols xs = colDft rows cols . rowDft rows cols $ xs


dft::Vector (Complex Double) -> Vector (Complex Double)
dft xs = DV.map (\k -> sum (imap (arg k) xs)) ((numvec n)::Vector (Int))
  where
    n = length xs
    nf = fromIntegral n
    numvec n = enumFromStepN 0 1 n
    arg k i x = x * exp (-2 * pi * kf * ifl * (0:+1)/nf)
              where
                kf = fromIntegral k
                ifl = fromIntegral i
      
idft::Vector (Complex Double) -> Vector (Complex Double)
idft xs = DV.map (\k ->(/) (sum (imap (arg k) xs)) nf) ((numvec n)::Vector (Int))
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
                  xse = DV.map ((!) xs) (enumFromStepN 0 2 m)
                  xso = DV.map ((!) xs) (enumFromStepN 1 2 m)
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
                   xtop_scaled = DV.map (/2) xtop
                   xbottom = zipWith3 fft_bottom fxse fxso (enumFromStepN 0 1 m)
                   xbottom_scaled = DV.map (/2) xbottom
                   xse = DV.map ((!) xs) (enumFromStepN 0 2 m)
                   xso = DV.map ((!) xs) (enumFromStepN 1 2 m)
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
      