{-# LANGUAGE ScopedTypeVariables #-}
import Criterion.Main
import Complex
-- import Data.List.Split
import Data.Vector 
import Data.ByteString.Lazy.Char8 as BS
import System.IO
import Maybe

complexProd::RealFloat a => a -> a -> a -> a -> (a, a)
complexProd a_r a_c b_r b_c = (a_r * b_r - a_c * b_c, a_c * b_r + a_r * b_c)

complexSum::RealFloat a=> a -> a -> a -> a -> (a ,a)
complexSum a_r a_c b_r b_c = (a_r + b_r, a_c + b_c)

-- fft::[Float] -> [[Float]]
fft::Vector (Complex Double) -> Vector (Complex Double)
fft xs = if (Data.Vector.length xs) == 1 then
           xs
         else
           let n = Data.Vector.length xs
               m = n `div` 2
               y_top = fft (Data.Vector.map (xs Data.Vector.!) . Data.Vector.filter (even) . Data.Vector.enumFromTo 0 $ (n-1))
               y_bottom = fft (Data.Vector.map (xs Data.Vector.!) . Data.Vector.filter (odd) . Data.Vector.enumFromTo 0 $ (n-1))
               n_f = fromIntegral n
               arg = (\x -> -2 * pi * x / n_f)
               -- wn = exp (-2 * pi * (0:+1)/(fromIntegral n))
               wn_r = (\x -> cos (-2 * pi * x/n_f))
               wn_c = (\x -> sin (-2 * pi * x/n_f))
               -- cospart = Data.Vector.map (\x -> cos . arg $ x) (Data.Vector.enumFromTo 0 (m-1))
               -- sinpart = Data.Vector.map (\x -> sin . arg $ x) (Data.Vector.enumFromTo 0 (m-1))
               -- d = Data.Vector.map (wn^) (Data.Vector.enumFromTo 0 (m-1))
               d_r = Data.Vector.map wn_r (Data.Vector.enumFromTo 0 (m-1))
               d_c = Data.Vector.map wn_c (Data.Vector.enumFromTo 0 (m-1))
               -- z = Data.Vector.zipWith (*) d y_bottom
               z = Data.Vector.zipWith4 complexProd (fst d) (snd d) (fst y_bottom) (snd y_bottom)
           in
            (Data.Vector.zipWith4 (complexSum) (fst y_top) (snd y_top) (fst z) (snd z)) Data.Vector.++ (Data.Vector.zipWith4 (\ar ac br bc -> complexSum ar ac br (-bc)) (fst y_top) (snd y_top) (fst z) (snd z)))
            

ifft::Vector (Complex Double) -> Vector (Complex Double)
ifft xs = if (Data.Vector.length xs) == 1 then
           xs
         else
           let n = Data.Vector.length xs
               m = n `div` 2
               y_top = ifft (Data.Vector.map (xs Data.Vector.!) . Data.Vector.filter (even) . Data.Vector.enumFromTo 0 $ (n-1))
               y_bottom = ifft (Data.Vector.map (xs Data.Vector.!) . Data.Vector.filter (odd) . Data.Vector.enumFromTo 0 $ (n-1))
               wn = exp (2 * pi * (0:+1)/(fromIntegral n))
               d = Data.Vector.map (wn^) (Data.Vector.enumFromTo 0 (m-1))
               z = Data.Vector.zipWith (*) d y_bottom
           in
            (Data.Vector.zipWith (+) y_top z) Data.Vector.++ (Data.Vector.zipWith (+) y_top (Data.Vector.map ((-1)*) z))

splitEvery n xs | Data.Vector.null xs = Data.Vector.empty
                | otherwise = Data.Vector.cons (Data.Vector.take n xs)  (splitEvery n (Data.Vector.drop n xs))

headerSize bs = (+) 4 (BS.length . BS.concat . Prelude.take 4 . BS.lines $ bs)

scale val sf = val/sf
            
dropHeader bs = BS.drop (headerSize bs) $ bs

bstovec bs = Data.Vector.fromList . BS.lines . dropHeader $ bs

tofloatvec v = Data.Vector.map (fromRational . fromIntegral . fst . fromJust) . Data.Vector.map (BS.readInt) $ v

-- main = do
--  a <- BS.readFile "16x16.pgm"
--  a <- BS.readFile "aish_gray_512x320.pgm"
--  a <- BS.readFile "hs-logo.pgm"
  -- let avec = bstovec a
  --     favec = tofloatvec avec
  --      fftavec = fft favec
-- main = defaultMain [
--   bench "fft hs-logo.pm " $ nf (fft) bench_input
--   ]
 --      ifftavec = ifft fftavec

wtf = toList . Data.Vector.map (round . realPart) . ifft . fft . Data.Vector.map (\x -> (x/(fromIntegral (Data.Vector.length bench_input)):+0))

main = do 
  a <- BS.readFile "aish_gray_512x512.pgm"
  let fvec = tofloatvec . bstovec $ a
  print (show . Data.Vector.length $ fvec)
  defaultMain [ 
    bench "wtf" $ whnf wtf fvec
    ]

 -- System.IO.writeFile "out.txt" $ Data.Vector.foldl' (\f n -> f Prelude.++ ['\n'] Prelude.++ n) "" . Data.Vector.map (\val -> show . round . realPart $ scale val (fromIntegral . Data.Vector.length $ avec)) $ ifftavec 


                    