module BStoVec (lastCharofChunk, takeChunks, headerSize, bsToVec) where

import Data.ByteString.Lazy.Char8 as BS
import Data.Vector as DV
import Data.Maybe

lastCharofChunk chunkSz bs = BS.last . BS.take chunkSz $ bs
takeChunks chunkSz bs = if lastCharofChunk chunkSz bs /= '\n' then 
                          takeChunks (chunkSz+1) bs 
                        else 
                          BS.splitAt chunkSz bs
headerSize bs = (+) 4 (BS.length . BS.concat . Prelude.take 4 . BS.lines $ bs)

bsToVec sz bs = bsToVec' sz (BS.drop (headerSize bs) bs) DV.empty 
  where
    bsToVec' sz body vec | body /= BS.empty = bsToVec' sz rest (vec DV.++ (DV.fromList . BS.split '\n' $ chunk)) 
                         | otherwise = DV.map (fst . fromJust . BS.readInt) . DV.filter (/= BS.empty) $ vec
      where
        (chunk, rest) = takeChunks sz body
        