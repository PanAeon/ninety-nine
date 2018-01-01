{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

--  stack --no-nix-pure exec -- pretty-pictures-exe


import Control.Comonad
import Control.Lens as L
import Data.Bits
import Data.Bits.Lens as L
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.MemoCombinators
import Data.Word
import Diagrams.Backend.SVG
import Diagrams.Prelude as D
import Yesod
import Graphics.Svg.Core(renderBS)

data Store s a = Store (s -> a) s deriving Functor

instance Comonad (Store s) where
   extract (Store f s) = f s
   duplicate (Store f s) = Store (Store f) s

-- characterizes store completely
experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment k (Store f s) = f <$> k s

-- Store gives you a little bit more power than we want in a cellular automaton, as you can do both relative and global addressing,

-- FIXME: understood nothing below, should revisit in a couple of months (but fuck this yesod part)


rule :: Num s => Word8 -> Store s Bool -> Bool
rule w (Store f s) = testBit w $
  0 L.& partsOf (taking 3 L.bits) .~ [f (s+1), f s, f (s-1)]

tab :: Memo s -> Store s a -> Store s a
tab opt (Store f s) = Store (opt f) s

loop :: Integral s => (Store s a -> a) -> Store s a -> [Store s a]
loop f = iterate (extend f . tab integral)

-- show
window :: (Enum s, Num s) => s -> s -> Store s a -> [a]
window l h = experiment $ \ s -> [s-l..s+h]

xo :: Bool -> Char
xo True  = 'X'
xo False = ' '


tmain = mapM_ (putStrLn . map xo . window 50 0) $
       take 50 $ loop (rule 110) $ Store (==0) 0

grid :: [[Bool]] -> Diagram SVG
grid = cat unitY . reverse . map (hcat . map cell) where
  cell b = unitSquare D.# fc (if b then black else white)

svg :: Diagram SVG  -> Strict.ByteString
svg = Lazy.toStrict . renderBS . renderDia SVG (SVGOptions (mkWidth 400) Nothing "" [] True)



data App = App

instance Yesod App

mkYesod "App" [parseRoutes| / ImageR GET |]

getImageR :: MonadHandler m => m TypedContent
getImageR = sendResponse $ toTypedContent (typeSvg, toContent img)

img = svg . grid . map (window 99 0) . take 100 . loop (rule 110) $ Store (==0) (0 :: Int)

main = warpEnv App
