{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Lens.WikiTutorial() where

-- stack exec ghci
--  :load WikiTutorial.hs
-- reference: https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references
import           Control.Lens
import           Control.Lens.Setter

data Point = Point {
    _positionX :: Double
  , _positionY :: Double
} deriving Show
makeLenses ''Point

data Segment = Segment {
    _segmentStart :: Point
  , _segmentEnd   :: Point
} deriving Show
makeLenses ''Segment

makePoint :: (Double, Double) -> Point
makePoint (x,y) = Point x y

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment a b = Segment (makePoint a) (makePoint b)


testSeg = makeSegment (1,2) (4,6)

foo = over (segmentEnd . positionY) (2*) testSeg

foo'  = view (segmentEnd . positionY) testSeg
foo'' = set  (segmentEnd . positionY) (-4.0) testSeg

pointCoordinates:: Applicative f => (Double -> f Double) -> Point -> f Point
pointCoordinates g (Point x y) = Point <$> g x <*> g y

extremityCoordinates :: Applicative f => (Double -> f Double) -> Segment -> f Segment
extremityCoordinates g (Segment a b) = Segment <$> g' a <*> g' b
            where
              g' = pointCoordinates g

scaleSegment :: Double -> Segment -> Segment
scaleSegment d = over extremityCoordinates (*d)

 -- type Setter s t a b = forall f. Settable f => (a -> f b) -> s -> f t
 -- (a -> f b) -> (f a) -> f (f b)
mapped' :: Functor f => Setter (f a) (f b) a b
mapped' = sets fmap -- :: ((a -> b) -> s -> t) -> Setter s t a b
  --Identity $ fmap undefined ma
