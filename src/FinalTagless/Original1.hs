 {-# LANGUAGE ExistentialQuantification #-}
module FinalTagless.Original1
    (
    ) where

import qualified Data.Map.Lazy as Map
{-
Paper: Finally Tagless, Partially Evaluated
Jacques Carette, Oleg Kiselyov and Chung-chieh Shan
http://okmij.org/ftp/tagless-final/JFP.pdf
Code examples:
http://okmij.org/ftp/tagless-final/
-}

-- initial encoding
data Var = VZ | VS Var
data Exp = V Var | B Bool | L Exp | A (Exp, Exp)

data U = UB Bool | UA (U -> U)

instance Show U where
  show (UB b) = "UB " ++ show b
  show (UA f) = "UA f -> f"

-- (λx.x)True
test1 = A(L (V VZ) , B True)

lookup' (x:env) VZ = x
lookup' (x:env) (VS v) = lookup' env v

eval0 :: [U] -> Exp -> U
eval0 env term = case term of
  V v -> lookup' env v
  B b -> UB b
  L e -> UA (\x ->  eval0 (x:env) e )
  A (e1, e2) -> case (eval0 env e1) of
                  UA f ->  f (eval0 env e2)

test1r = eval0 [] test1 -- UB True

test2 = A (B True, B False)
test2r = eval0 [] test2 -- match error (can't apply boolean to boolean)

test3 = A (L (V (VS VZ)), B True)
test3r = eval0 [] test3 -- exception, open term above

-- Following an old idea of Reynolds (1975), we represent object programs using ordinary
-- functions rather than data constructors.

varZ env = fst env
varS vp env = vp (snd env)
b bv env = bv
lam e env  = \x -> e (x,env)
app e1 e2 env = (e1 env) (e2 env)

-- (λx.x)True

testf1 = app (lam varZ) (b True)
testf1r = testf1 []

testf3 = app (lam (varS varZ)) (b True)

-- testf3 [] -> Couldn't match expected type `(t, b0)' with actual type `[t0]'


-- e call this approach
{-
  "We call this approach final (rather then initial),
  because we represent each object term not by its
  abstract syntax, but by its denotation in
  semantic algebra"
-}

-- “a way to write a typed fold function over a typed term.”

-- So, that was a "simple" idea, then abstracting
-- over it makes it much more interesting

-- all right, understood that I haven't really understood anything...
-- so idea, even more simplified

num x = x
add x y = x + y
mul x y = x * y

testg1 = add (num 2) (mul (num 3) (num 5))

-- then we add var end env (wow, this is difficult)
-- all right use unsafe Map and define vars as symbols
-- not vars but lets, with vars could be only numbers
num' x env = x
add' x y env = (x env) + (y env)
let' n v e env = e (Map.insert n (v env) env )
var' n env = maybe (error "not found") id (Map.lookup n env)

-- yeah, lambda calculus got simpler than arythmetics

testg2 = let' "x" (num' (3::Int)) (add' (num' (4::Int)) (var' "x"))
