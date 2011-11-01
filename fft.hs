import Complex
-- import Data.List.Split
import Data.Vector 
import Data.ByteString.Lazy.Char8 as BS
import System.IO
import Maybe


-- fft::[Float] -> [[Float]]
fft::Vector (Complex Double) -> Vector (Complex Double)
fft xs = if (Data.Vector.length xs) == 1 then
           xs
         else
           let n = Data.Vector.length xs
               m = n `div` 2
               -- y_top = fft (Data.Vector.map Data.Vector.head (splitEvery 2 xs))
               -- y_bottom = fft (Data.Vector.map Data.Vector.last (splitEvery 2 xs))
               y_top = ifft (Data.Vector.map (xs Data.Vector.!) . Data.Vector.filter (even) . Data.Vector.enumFromTo 0 $ (n-1))
               y_bottom = ifft (Data.Vector.map (xs Data.Vector.!) . Data.Vector.filter (odd) . Data.Vector.enumFromTo 0 $ (n-1))
               wn = exp (-2 * pi * (0:+1)/(fromIntegral n))
               d = Data.Vector.map (wn^) (Data.Vector.enumFromTo 0 (m-1))
               z = Data.Vector.zipWith (*) d y_bottom
           in
            -- Data.Vector.concat [(Data.Vector.zipWith (+) y_top z), (Data.Vector.zipWith (+) y_top (Data.Vector.map ((-1)*) z))]
            (Data.Vector.zipWith (+) y_top z) Data.Vector.++ (Data.Vector.zipWith (+) y_top (Data.Vector.map ((-1)*) z))
            

ifft::Vector (Complex Double) -> Vector (Complex Double)
ifft xs = if (Data.Vector.length xs) == 1 then
           xs
         else
           let n = Data.Vector.length xs
               m = n `div` 2
               -- y_top = ifft (Data.Vector.map Data.Vector.head (splitEvery 2 xs))
               -- y_bottom = ifft (Data.Vector.map Data.Vector.last (splitEvery 2 xs))
               y_top = ifft (Data.Vector.map (xs Data.Vector.!) . Data.Vector.filter (even) . Data.Vector.enumFromTo 0 $ (n-1))
               y_bottom = ifft (Data.Vector.map (xs Data.Vector.!) . Data.Vector.filter (odd) . Data.Vector.enumFromTo 0 $ (n-1))
               wn = exp (2 * pi * (0:+1)/(fromIntegral n))
               d = Data.Vector.map (wn^) (Data.Vector.enumFromTo 0 (m-1))
               z = Data.Vector.zipWith (*) d y_bottom
           in
            -- Data.Vector.concat [(Data.Vector.zipWith (+) y_top z), (Data.Vector.zipWith (+) y_top (Data.Vector.map ((-1)*) z))]
            (Data.Vector.zipWith (+) y_top z) Data.Vector.++ (Data.Vector.zipWith (+) y_top (Data.Vector.map ((-1)*) z))

splitEvery n xs | Data.Vector.null xs = Data.Vector.empty
                | otherwise = Data.Vector.cons (Data.Vector.take n xs)  (splitEvery n (Data.Vector.drop n xs))

headerSize bs = (+) 4 (BS.length . BS.concat . Prelude.take 4 . BS.lines $ bs)

scale val sf = val/sf
            
dropHeader bs = BS.drop (headerSize bs) $ bs

bstovec bs = Data.Vector.fromList . BS.lines . dropHeader $ bs

tofloatvec v = Data.Vector.map (fromRational . fromIntegral . fst . fromJust) . Data.Vector.map (BS.readInt) $ v

main = do
--  a <- BS.readFile "16x16.pgm"
--  a <- BS.readFile "aish_gray_512x320.pgm"
  a <- BS.readFile "hs-logo.pgm"
  let avec = bstovec a
      favec = tofloatvec avec
      fftavec = fft favec
      ifftavec = ifft fftavec

  System.IO.writeFile "out.txt" $ Data.Vector.foldl' (\f n -> f Prelude.++ ['\n'] Prelude.++ n) "" . Data.Vector.map (\val -> show . round . realPart $ scale val (fromIntegral . Data.Vector.length $ avec)) $ ifftavec 


-- main = do
--   a <- BS.readFile "aish_gray_512x320.pgm"
--   print . Data.Vector.map ((\val -> if val > 255 then 255 else val) . round . Complex.realPart) . Data.Vector.map (/255) . ifft . fft . Data.Vector.map (fromRational . fromIntegral) . bsToVec 512 $ a

                   
-- main = do 
--   -- xs = [4,0,3,6,2,9,6,5,4,0,3,6,2,9,6,5]
--   a <- readFile "aish_gray_512x320.pgm"
--   print xs >> print (fft $ xs) >> print (Data.Vector.map (floor . toRational) . Data.Vector.map (realPart) . Data.Vector.map (/nf) . ifft . fft $ xs)
--   where
--     xs = Data.Vector.map (read::String -> Complex Double) . drop 4 . lines $ a
--     nf = fromIntegral . Data.Vector.length $ xs
--     oxs = Data.Vector.map (floor . toRational) . Data.Vector.map (realPart) . Data.Vector.map (/(512*320)) . ifft . fft xs
                    