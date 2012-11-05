{-# LANGUAGE BangPatterns, NoMonomorphismRestriction #-}
 
import qualified Data.ByteString.Lazy.Char8      as BS
import qualified Data.ByteString.Lex.Lazy.Double as BS
import qualified Data.Vector.Unboxed as DV
import qualified Data.Vector as BV
import Prelude hiding (length, sum, zipWith3)
import Data.Vector.Unboxed as DU
import qualified Data.Vector.Algorithms.Intro as DV
import Data.Complex
import System.Environment
import Control.Monad
import Data.Binary.Get
import Data.Maybe

headerSize bs = (+) 4 (BS.length . BS.concat . Prelude.take 4 . BS.lines $ bs)  
  
dropHeader bs = BS.drop (headerSize bs) bs

main = do
    [f] <- getArgs
    s   <- BS.readFile f
    let [width,height] = Prelude.map (fst . fromJust . BS.readInt) (BS.words $ (BS.lines s) !! 2)
    putStrLn "P2"
    putStrLn "# Created by haskell-dft"
    putStrLn $ (show width) Prelude.++ " " Prelude.++ (show height)
    -- putStrLn "255"
    -- DV.mapM print . map ((round::RealFrac a => a -> Int) . magnitude) . (fft2D 512 512) . DV.map (\x -> (x:+0)) . parse $ (dropHeader s)
    -- DV.map (round::RealFrac a => a -> Int) .
    --  . scale .
    -- let xs =  DV.map (round::RealFrac a => a -> Int) . DV.map (log . (+) 1 . magnitude) . fft2D 512 512 . DV.map (\x -> (x:+0)) . parse $ dropHeader s
    let xs =  DV.map (round::RealFrac a => a -> Int) . DV.map (log . (+) 1 . magnitude) . fft2D width height . DV.map (:+0) . parse $ dropHeader s
    
    print (DV.maximum xs)
    DV.mapM print xs
    
scale::Vector Double -> Vector Double
scale xs = DV.cons 255.0 (DV.map (\x -> (x/maxVec)*255.0) (DV.tail xs))
  where
    maxVec = DV.maximum . DV.tail $ xs -- Take one less than the max, the first value is the max (DC value?)
    
        

-- findMax::Vector Double -> Double
-- findMax xs = findMax' xs 0
--              where
--                findMax' xs max | DV.null xs = max
--                                | x > max = findMax' txs x
--                                | otherwise = findMax' txs max
--                                  where
--                                    x = head xs
--                                    txs = tail xs
                                   
rowDft::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
rowDft rows cols b = DV.concatMap (fftCT . getRow) starts
                     where
                       starts = DV.enumFromStepN 0 cols rows
                       getRow x = DV.slice x (fromIntegral cols) b

colDft::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
colDft rows cols b = DV.concatMap (fftCT . getCol) starts
                     where
                       starts = DV.enumFromStepN 0 1 cols
                       getCol x = DV.map ((DV.!) b) (DV.enumFromStepN x cols rows)

fft2D::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
fft2D rows cols xs = colDft rows cols . rowDft rows cols $ xs


rowiDft::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
rowiDft rows cols b = DV.concatMap (ifftCT . getRow) starts
                     where
                       starts = DV.enumFromStepN 0 cols rows
                       getRow x = DV.slice x (fromIntegral cols) b

coliDft::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
coliDft rows cols b = DV.concatMap (ifftCT . getCol) starts
                     where
                       starts = DV.enumFromStepN 0 1 cols
                       getCol x = DV.map ((DV.!) b) (DV.enumFromStepN x cols rows)

ifft2D::Int -> Int -> Vector (Complex Double) -> Vector (Complex Double)
ifft2D rows cols xs = colDft rows cols . rowDft rows cols $ xs


dft::Vector (Complex Double) -> Vector (Complex Double)
dft xs = DV.map (\k -> sum (imap (arg k) xs)) (numvec n :: Vector Int)
  where
    n = length xs
    nf = fromIntegral n
    numvec = enumFromStepN 0 1
    arg k i x = x * exp (-2 * pi * kf * ifl * (0:+1)/nf)
              where
                kf = fromIntegral k
                ifl = fromIntegral i
      
idft::Vector (Complex Double) -> Vector (Complex Double)
idft xs = DV.map (\k ->(/) (sum (imap (arg k) xs)) nf) (numvec n :: Vector Int)
  where
    n = length xs
    nf = fromIntegral n
    numvec = enumFromStepN 0 1
    arg k i x = x * exp (2 * pi * kf * ifl * (0:+1)/nf)
              where
                kf = fromIntegral k
                ifl = fromIntegral i

fftCT xs = if length xs == 1 then
              dft xs
            else
              let xtop = zipWith3 fft_top fxse fxso (enumFromStepN 0 1 m)
              
                  xbottom = zipWith3 fft_bottom fxse fxso (enumFromStepN 0 1 m)
                  xse = DV.map (xs !) (enumFromStepN 0 2 m)
                  xso = DV.map (xs !) (enumFromStepN 1 2 m)
                  fxse = fftCT xse
                  fxso = fftCT xso
                  n = length xs
                  nf = fromIntegral n
                  m = n `div` 2
                  fft_top xe xo k = xe + xo * exp (-2 * (0:+1) * pi * k / nf)
                  fft_bottom xe xo k = xe - xo * exp (-2 * (0:+1) * pi * k / nf)
              in              
                  (DU.++) xtop xbottom

ifftCT xs = if length xs == 1 then
               idft xs
            else
               let xtop = zipWith3 fft_top fxse fxso (enumFromStepN 0 1 m)
                   xtop_scaled = DV.map (/2) xtop
                   xbottom = zipWith3 fft_bottom fxse fxso (enumFromStepN 0 1 m)
                   xbottom_scaled = DV.map (/2) xbottom
                   xse = DV.map (xs !) (enumFromStepN 0 2 m)
                   xso = DV.map (xs !) (enumFromStepN 1 2 m)
                   fxse = ifftCT xse
                   fxso = ifftCT xso
                   n = length xs
                   nf = fromIntegral n
                   m = n `div` 2
                   fft_top xe xo k = xe + xo * exp (2 * (0:+1) * pi * k / nf)
                   fft_bottom xe xo k = xe - xo * exp (2 * (0:+1) * pi * k / nf)
              in              
                (DU.++) xtop_scaled xbottom_scaled


-- Fill a new vector from a file containing a list of numbers.
parse = DV.unfoldr step
  where
    step !s = case BS.readDouble s of
      Nothing       -> Nothing
      Just (!k, !t) -> Just (k, BS.tail t)
      