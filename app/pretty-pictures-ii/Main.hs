{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}



import Codec.Compression.Zlib
import Control.Lens.Internal.Context
import qualified Control.Lens as L
import Data.Bits
import qualified Data.Bits.Lens as L
import Data.Monoid
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Vector.Unboxed as Unboxed
import Data.Foldable as F
import Data.MemoTrie
import Yesod


import Control.Applicative
import Data.Profunctor
import Control.Comonad
import Control.Exception
import Control.Monad


data Fold a b = forall x . Fold (x -> a -> x) x (x -> b)

instance Functor (Fold a) where
  fmap f (Fold rar r rb) = Fold rar r (f.rb)

data Pair a b = Pair !a !b

instance Applicative (Fold a) where
  pure b = Fold (\() _ -> ()) () (\() -> b)
  {-# INLINABLE pure #-}
  Fold sas s0 s2f <*> Fold rar r0 r2x = Fold
    (\(Pair s r) a -> Pair (sas s a) (rar r a))
    (Pair s0 r0)
    (\(Pair s r) -> s2f s (r2x r))
  {-# INLINABLE (<*>) #-}

instance Num b => Num (Fold a b) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional b => Fractional (Fold a b) where
  recip = fmap recip
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance Profunctor Fold where
  dimap f g (Fold rar r0 rb) = Fold (\r -> rar r . f) r0 (g . rb)

-- data Fold a b = forall x . Fold (x -> a -> x) x (x -> b)
instance Comonad (Fold a) where
  extract (Fold xax x0 xb) = xb x0
  duplicate (Fold xax x0 xb) = Fold xax x0 (\x1 -> Fold xax x1 xb)

-- FIXME: what is ComonadApply ?
instance ComonadApply (Fold a) where
  (<@>) = (<*>)

-- works with codo -- see A Notation for Comonads Dominic Orchard a

data Env' e a = Env' e a deriving (Eq,Ord,Show,Read,Functor)


instance Comonad (Env' e) where
  extract (Env' e a) = a
  duplicate (Env' e a) = Env' e (Env' e a)

data Moore b a = Moore a (b -> Moore b a) deriving Functor

-- Moore is a Cofree Comonad!
-- That is to say, Moore b a ~ Cofree ((->) b) a.

data YonedaMoore a b = forall r. YonedaMoore (r -> a) (Moore a r) -- FUCK, at last you have told apply Yoneda Lema FIXME: apply yoneda to Store !



instance Comonad (Moore b) where
  extract (Moore x f) = x
  duplicate m@(Moore a f) = Moore m (duplicate <$> f)
  extend f w@(Moore _ as)  = Moore (f w) (extend f <$> as)




-- Moore b (Moore b a)
-- Moore (Moore b a) (b -> Moore (Moore b a) a)

main' = do
  test "extract" $ extract (Env' 1 2) == 2
  test "duplicate" $ duplicate (Env' 1 2) == Env' 1 (Env' 1 2)
  test "extract" $ 1 == extract (Moore 1 $ error "you don't need to look in the tail")

test :: String -> Bool -> IO ()
test s b = try (return $! b) >>= \ ec -> case ec of
  Left (e :: SomeException) -> putStrLn $ s ++ " failed: " ++ show e
  Right True -> putStrLn $ s ++ " is correct!"
  Right False -> putStrLn $ s ++ " is not correct!"




rule :: Num s => Word8 -> Context s s Bool -> Bool
rule w (Context f s) = testBit w $ 0 L.& L.partsOf L.bits L..~ [f (s+1), f s, f (s-1)]

loop :: HasTrie s => (Context s s a -> a) -> Context s s a -> [Context s s a]
loop f = iterate (tab . extend f) . tab where
  tab (Context k s) = Context (memo k) s

data L b a = forall x. L (x -> b -> x) x (x -> a)

more :: Lazy.ByteString -> L Word8 a -> a
more bs (L xbx x xa) = xa (Lazy.foldl' xbx x bs)

crc32 :: L Word8 Word32
crc32 = L step 0xffffffff complement where
  step r b = unsafeShiftR r 8 `xor` crcs Unboxed.! fromIntegral (xor r (fromIntegral b) .&. 0xff)

crcs :: Unboxed.Vector Word32
crcs = Unboxed.generate 256 (go.go.go.go.go.go.go.go.fromIntegral) where
  go c = unsafeShiftR c 1 `xor` if c .&. 1 /= 0 then 0xedb88320 else 0

putChunk :: Lazy.ByteString -> Lazy.ByteString -> Put
putChunk h b = do
  putWord32be $ fromIntegral (Lazy.length b)
  putLazyByteString h
  putLazyByteString b
  putWord32be $ more (h <> b) crc32

png :: Int -> Int -> [Int -> (Word8, Word8, Word8)] -> Lazy.ByteString
png w h fs = runPut $ do
  putLazyByteString "\x89PNG\r\n\x1a\n"
  putChunk "IHDR" $ runPut $ do
    putWord32be (fromIntegral w)
    putWord32be (fromIntegral h)
    putWord8 8 -- 8 bit color depth
    putWord8 2 -- RGB
    putWord8 0
    putWord8 0
    putWord8 0
  putChunk "IDAT" $
    compressWith defaultCompressParams { compressLevel = bestSpeed } $
    runPut $ forM_ (take h fs) $ \f -> do
      putWord8 0
      forM_ [0..w-1] (put . f)
  putChunk "IEND" mempty

data App = App
instance Yesod App
mkYesod "App" [parseRoutes| / ImageR GET |]
main = warpEnv App
-- /show

-- show
getImageR :: MonadHandler m => m TypedContent
getImageR = sendResponse $ toTypedContent (typePng, toContent img) where
  img = png 150 150 $ draw <$> loop (rule 110) (Context (==149) 149)
  draw (Context p _) x = if p x then (0,0,0) else (255,255,255)
-- /show
