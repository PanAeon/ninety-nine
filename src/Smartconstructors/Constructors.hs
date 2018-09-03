
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Smartconstructors.Constructors() where

--
-- from https://markkarpov.com/post/smart-constructors-that-cannot-fail.html
--
-- hmm, interesting, but blog post is not very consistent, and
-- I'm not sure about applications of this ..


import           Control.Monad
-- import           Control.Monad.Catch        (Exception (..), MonadThrow (..))
import           Control.Monad.Except (MonadError (..))
import qualified Control.Monad.Fail   as Fail
import           Data.Coerce
import           Data.Kind
import           Data.Proxy
import           Data.Typeable        (Typeable)
import           GHC.Generics
import           GHC.Stack
import           GHC.TypeLits
-- import qualified Language.Haskell.TH.Syntax as TH



newtype GreaterThenFive = GreaterThenFive Int

mkGreaterThenFive :: Int -> Maybe GreaterThenFive
mkGreaterThenFive x = if x > 5
                      then Just $ GreaterThenFive x
                      else Nothing

unGreaterThanFive :: GreaterThenFive -> Int
unGreaterThanFive (GreaterThenFive x) = x


-------------------------------------

newtype Refined (ps :: [*]) a = Refined a

refined :: a -> Refined '[] a
refined = Refined

unrefined :: Refined ps a -> a
unrefined (Refined a) = a

class Prop a p where
  type PropProjection a p :: *
  checkProp :: Proxy p -> a -> Either String (PropProjection a p)

data NotEmpty

data GreaterThan (n :: Nat)

instance (Integral a, KnownNat n) => Prop a (GreaterThan n) where
  type PropProjection a (GreaterThan n) = a
  checkProp Proxy n =
    if n > fromIntegral (natVal (Proxy :: Proxy n))
    then Right n
    else Left "not your day"
