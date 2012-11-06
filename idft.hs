{-# LANGUAGE BangPatterns, NoMonomorphismRestriction #-}
 
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lex.Double as BS
import qualified Data.Vector.Unboxed as DV
import qualified Data.Vector as BV
import Prelude hiding (length, sum, zipWith3)
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
    putStrLn "255"
    let xs = fft2D width height . DV.map (:+0) . parse $ dropHeader s
    DV.mapM (print . ((round::RealFrac a => a -> Int) . realPart)) (ifft2D width height xs)
    
scale::DV.Vector Double -> DV.Vector Double
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
                                   
rowDft::Int -> Int -> DV.Vector (Complex Double) -> DV.Vector (Complex Double)
rowDft rows cols b = DV.concatMap (fftCT . getRow) starts
                     where
                       starts = DV.enumFromStepN 0 cols rows
                       getRow x = DV.slice x (fromIntegral cols) b

colDft::Int -> Int -> DV.Vector (Complex Double) -> DV.Vector (Complex Double)
colDft rows cols b = DV.concatMap (fftCT . getCol) starts
                     where
                       starts = DV.enumFromStepN 0 1 cols
                       getCol x = DV.map ((DV.!) b) (DV.enumFromStepN x cols rows)

fft2D::Int -> Int -> DV.Vector (Complex Double) -> DV.Vector (Complex Double)
fft2D rows cols xs = colDft rows cols . rowDft rows cols $ xs


rowiDft::Int -> Int -> DV.Vector (Complex Double) -> DV.Vector (Complex Double)
rowiDft rows cols b = DV.concatMap (ifftCT . getRow) starts
                     where
                       starts = DV.enumFromStepN 0 cols rows
                       getRow x = DV.slice x (fromIntegral cols) b

coliDft::Int -> Int -> DV.Vector (Complex Double) -> DV.Vector (Complex Double)
coliDft rows cols b = DV.concatMap (ifftCT . getCol) starts
                     where
                       starts = DV.enumFromStepN 0 1 cols
                       getCol x = DV.map ((DV.!) b) (DV.enumFromStepN x cols rows)

ifft2D::Int -> Int -> DV.Vector (Complex Double) -> DV.Vector (Complex Double)
ifft2D rows cols xs = coliDft rows cols . rowiDft rows cols $ xs


dft::DV.Vector (Complex Double) -> DV.Vector (Complex Double)
dft xs = DV.map (\k -> DV.sum (DV.imap (arg k) xs)) (numvec n :: DV.Vector Int)
  where
    n = DV.length xs
    nf = fromIntegral n
    numvec = DV.enumFromStepN 0 1
    arg k i x = x * exp (-2 * pi * kf * ifl/nf)
              where
                kf = fromIntegral k
                ifl = fromIntegral i
      
idft::DV.Vector (Complex Double) -> DV.Vector (Complex Double)
idft xs = DV.map (\k ->(/) (DV.sum (DV.imap (arg k) xs)) nf) (numvec n :: DV.Vector Int)
  where
    n = DV.length xs
    nf = fromIntegral n
    numvec = DV.enumFromStepN 0 1
    arg k i x = x * exp (2 * pi * kf * ifl/nf)
              where
                kf = fromIntegral k
                ifl = fromIntegral i

fftCT xs = if DV.length xs == 1 then
              dft xs
            else
              let xtop = DV.zipWith3 fft_top fxse fxso (DV.enumFromStepN 0 1 m)
              
                  xbottom = DV.zipWith3 fft_bottom fxse fxso (DV.enumFromStepN 0 1 m)
                  xse = DV.map (xs DV.!) (DV.enumFromStepN 0 2 m)
                  xso = DV.map (xs DV.!) (DV.enumFromStepN 1 2 m)
                  fxse = fftCT xse -- fftCT xse
                  fxso = fftCT xso -- fftCT xso
                  n = DV.length xs
                  nf = fromIntegral n
                  m = n `div` 2
                  fft_top xe xo k = xe + xo * exp (-2 * pi * k / nf)
                  fft_bottom xe xo k = xe - xo * exp (-2 * pi * k / nf)
              in              
                  (DV.++) xtop xbottom

ifftCT xs = if DV.length xs == 1 then
               idft xs
            else
               let xtop = DV.zipWith3 fft_top fxse fxso (DV.enumFromStepN 0 1 m)
                   xtop_scaled = DV.map (/2.0) xtop
                   xbottom = DV.zipWith3 fft_bottom fxse fxso (DV.enumFromStepN 0 1 m)
                   xbottom_scaled = DV.map (/2.0) xbottom
                   xse = DV.map (xs DV.!) (DV.enumFromStepN 0 2 m)
                   xso = DV.map (xs DV.!) (DV.enumFromStepN 1 2 m)
                   fxse = ifftCT xse
                   fxso = ifftCT xso
                   n = DV.length xs
                   nf = fromIntegral n
                   m = n `div` 2
                   fft_top xe xo k = xe + xo * exp (2 * pi * k / nf)
                   fft_bottom xe xo k = xe - xo * exp (2 * pi * k / nf)
              in              
                (DV.++) xtop_scaled xbottom_scaled
                -- (DV.++) xtop xbottom


-- Fill a new vector from a file containing a list of numbers.
parse = DV.unfoldr step
  where
    step !s = case BS.readDouble s of
      Nothing       -> Nothing
      Just (!k, !t) -> Just (k, BS.tail t)
      