{-# LANGUAGE KindSignatures, PolyKinds #-}
module FinalTagless.OriginalDeBruijn
    (
    ) where

-- hm, really simple indexing scheme:
-- https://en.wikipedia.org/wiki/De_Bruijn_index

varZ :: (a,b) -> a
varZ env = fst env

varS :: (a -> b) -> (c, a) -> b
varS vp env = vp (snd env)

b :: Bool -> t -> Bool
b bv env = bv

lam' :: ((a,h) -> b) -> h -> a -> b
lam' e env  = \x -> e (x,env)

app' :: (t2 -> t1 -> t) -> (t2 -> t1) -> t2 -> t
app' e1 e2 env = (e1 env) (e2 env)

test1' = app' (lam' varZ) (b True)
-- fuck it for the moment, let's represent at least variables with tags ...NO
-- how to extend above to repr ??


-- ( (a, t) -> b ) -> repr (a -> b)

class Symantics repr where
  bool :: Bool -> h -> repr Bool
  vz :: (repr a,b) -> repr a
  --vs :: ( a ->  b) -> (c,  a) ->  repr b
  app :: (h -> repr (a -> b)) -> (h -> repr a) -> h -> repr b
  lam ::  ((repr a,h) -> repr b)  -> h -> repr (a -> b)

-- (repr a,h) -> repr b

-- (a -> b) -> (c, a) -> b
-- (c, (a', b')) -> a'
-- (vs (repr a,h) -> repr b) -> (repr a,h) -> repr b
test1:: Symantics repr => h -> repr Bool
test1 = app (lam vz) (bool True)

-- test3 :: Symantics repr => (repr t,b) -> repr t
-- test3 = app (lam (vs vz)) (bool True)
mytest:: Symantics repr => h -> repr Bool
mytest = app (lam (app (lam (varS vz)) vz)) (bool True)

-- val vs  : ('c, 'h,'d) repr -> ('c, _ * 'h,'d) repr

data R a  = R {
   getR :: a
}

instance Symantics R where
  bool b _ = R b
  vz = fst
  app e1 e2 h = R $ getR (e1 h) (getR (e2 h))
  lam g h = R $ \a -> getR (g (R a, h))


-- FIXME: how to do this? simple example in Original 1 is more or less streightforward
-- class Symantics (repr :: a -> a -> a) where
--   --type H = repr
--   -- vz :: ((a,h) -> b) -> h -> repr a
--   vz :: (repr a, b) -> repr a
--   vs :: (b -> c) -> (repr a' , (repr b, t1)) -> repr c
--   bool :: Bool -> h -> repr Bool
--
--   lam  :: ((repr a,c) -> repr b ) -> h ->  repr (a -> b)
--   -- lam  :: ((a,h) -> b) ->  repr (a -> b)
--   app  :: repr (a -> b) h -> repr a h  -> repr b h
--
-- --val app  : ('c,'h,'da->'db) repr -> ('c,'h,'da) repr -> ('c,'h,'db) repr
-- test1:: Symantics repr => h -> repr Bool
-- test1 = app (lam vz) (bool True)



{-

data R a  = R {
   getR :: a
}
instance Symantics R where
  bool b _ = R (\_ -> b)
  -- vz (a, _) = a
  vz f = undefined
  --vs v (_, (a, _)) = R $ \h -> v (getR a h)

  -- let lam e = fun h -> fun x -> e (x,h)
  lam f = R (\h -> \x ->
              let
                -- rah = R (\h -> x)
                rab = f (x, h)
                -- b   = getR rab h
              in rab
            )
  -- let lam e = fun h -> fun x -> e (x,h)
  -- lam f = R (\h -> \x ->
  --             let
  --               rah = R (\h -> x)
  --               rab = f rah
  --             in getR rab (x,h)
  --           )
  app (R f) (R a) = R $ \h -> (f h) (a h)
-}
{-
module R = struct
type (’c,’h,’dv) repr = ’h -> ’dv
type (’c,’d) vr = ’d


let lam  f        h = fun x -> f (x,h)
let app  e1 e2    h = (e1 h) (e2 h)
let fix  f        h = let rec self n = f (self,h) n in self
let add  e1 e2    h = e1 h + e2 h
let mul  e1 e2    h = e1 h * e2 h
let leq  e1 e2    h = e1 h <= e2 h
let if_  eb et ee h = if eb h then et () h else ee () h
end
-}
