{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Lens.WikiTutorial() where

-- stack exec ghci
-- reference: https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references
import Control.Lens

data Point = Point {
    _positionX :: Double
  , _positionY :: Double
} deriving Show
makeLenses ''Point

data Segment = Segment {
    _segmentStart :: Point
  , _segmentEnd :: Point
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
