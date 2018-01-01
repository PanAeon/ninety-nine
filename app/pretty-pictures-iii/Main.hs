{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
import Codec.Compression.Zlib
import Control.Comonad
import Control.Lens as L
import Control.Lens.Internal.Context
import Data.Binary
import Data.Binary.Put
import Data.Bits
import Data.Bits.Lens as L
import Data.ByteString.Lazy as Lazy
import Data.Foldable as F
import Data.List as List
import Data.MemoCombinators
import Data.Monoid
import Data.Vector.Unboxed as Unboxed
import Yesod
 
-- show
type Rule m a = (m -> a) -> a

data Move = R | S | L deriving (Eq, Enum, Bounded, Show, Read)

rule :: (Enum m, Bounded m) => Word8 -> Rule m Bool
rule w f = testBit w $ 0 L.& partsOf L.bits .~ (f <$> [minBound .. maxBound])

type Act m s = m -> s -> s

modulo :: Int -> Act Move Int
modulo m L i = (i-1) `mod` m
modulo _ S i = i
modulo m R i = (i+1) `mod` m
-- /show

step :: Act m s -> Rule m a -> Context s s a -> a
step top r (Context f s) = r (f . flip top s)

loop :: Integral s => (Context s s a -> a) -> Context s s a -> [Context s s a]
loop f = List.iterate (tab . extend f) . tab where
  tab (Context g s) = Context (integral g) s

run :: Word8 -> Int -> Int -> [[Word8]]
run r x m0 = fmap line $ loop (step (modulo m0) (rule r)) $ Context (==0) 0 where
  line = fmap bw . window (x `div` 2)
  bw True  = 0
  bw False = 255
  window w = iexperiment $ \ s -> [s-w..s+w]

crc32 :: Lazy.ByteString -> Word32
crc32 = complement . Lazy.foldl' f 0xffffffff where
  f r b = unsafeShiftR r 8 `xor` crcs Unboxed.! fromIntegral (xor r (fromIntegral b) .&. 0xff)
  crcs = Unboxed.generate 256 (go.go.go.go.go.go.go.go.fromIntegral)
  go c = unsafeShiftR c 1 `xor` if c .&. 1 /= 0 then 0xedb88320 else 0

putChunk :: Lazy.ByteString -> Lazy.ByteString -> Put
putChunk h b = do
  putWord32be $ fromIntegral (Lazy.length b)
  putLazyByteString h
  putLazyByteString b
  putWord32be $ crc32 (h <> b)

png :: Int -> Int -> [[Word8]] -> Lazy.ByteString
png w h fs = runPut $ do
  putLazyByteString "\x89PNG\r\n\x1a\n"
  putChunk "IHDR" $ runPut $ do
    putWord32be (fromIntegral w)
    putWord32be (fromIntegral h)
    putWord8 8 -- 8 bit
    putWord8 0 -- greyscale
    putWord8 0
    putWord8 0
    putWord8 0
  putChunk "IDAT" $
    compressWith defaultCompressParams { compressLevel = bestSpeed } $
    runPut $ F.forM_ (Prelude.take h fs) $ \xs -> do
      putWord8 0
      F.forM_ (Prelude.take w xs) put
  putChunk "IEND" mempty

data App = App
instance Yesod App
mkYesod "App" [parseRoutes| / ImageR GET |]

main :: IO ()
main = warpEnv App

-- show
getImageR :: MonadHandler m => m TypedContent
getImageR = sendResponse $ toTypedContent (typePng, toContent img) where
  img = png 280 280 $ run 110 280 30
-- /show
