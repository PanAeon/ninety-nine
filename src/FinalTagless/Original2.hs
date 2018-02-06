{-# LANGUAGE GADTs, TypeOperators, ScopedTypeVariables #-}

module FinalTagless.Original2
    (
    ) where


-- "The class is so named because its interface gives the syntax of the object language
--  and its instances give the semantics. "

class Symantics repr where
  int :: Int -> repr Int
  bool :: Bool -> repr Bool
  lam  :: (repr a -> repr b) -> repr (a -> b)
  app  :: repr (a -> b) -> repr a -> repr b
  fix  :: (repr a -> repr a) -> repr a

  add  :: repr Int -> repr Int -> repr Int
  mul  :: repr Int -> repr Int -> repr Int
  leq  :: repr Int -> repr Int -> repr Bool
  if_  :: repr Bool -> repr a -> repr a -> repr a

-- (λx.x) true
test1:: Symantics repr => repr Bool
test1 = app (lam (\x -> x)) (bool True)



testpowfix () = lam (\x -> fix (\self -> lam (\n ->
       if_ (leq n (int 0)) (int 1)
       (mul x (app self (add n (int (-1)))))
       )))

-- does not terminate under R
cbnTest () = app (lam (\_ -> int 1)) (app (fix (\f -> f)) (int 2))
-- fixme: CBN interpreter !!!
-- λx.power x 7
testpowfix7 () = lam (\x ->
            app ( app (testpowfix () ) x)  (int 7))

-- The dummy argument () above is to avoid the monomorphism restriction,
data R a = R {
     unR :: a
}
instance Symantics R where
  int = R
  bool = R
  lam f = R $ \a -> unR $ f (R a)
  app (R f) (R a) = R $ f a
  fix f = f (fix f)
  add (R a) (R b) = R $ a + b
  mul (R a) (R b) = R $ a * b
  leq (R a) (R b) = R $ a <= b
  if_ (R c) (R _t) (R _f) = R $ if c then _t else _f

runTest1 :: Bool
runTest1 = unR test1

runTestPowFix7 :: Int
runTestPowFix7 = unR (testpowfix7 ()) 2

data L a = L {
  unL :: Int
}

instance Symantics L where
  int _ = L 1
  bool _ = L 1
  lam f  = L $ (unL $ f (L 0)) + 1
  app (L a) (L b) = L $ 1 + a + b
  fix f = L $  unL (f (L 0)) + 1
  add (L a) (L b) = L $ 1 + a + b
  mul (L a) (L b) = L $ 1 + a + b
  leq (L a) (L b) = L $ 1 + a + b
  if_ (L c) (L a) (L b) = L $ 1 + c + a + b


lengthTest1 :: Int
lengthTest1 = unL test1

lengthTestPowFix :: Int
lengthTestPowFix = unL (testpowfix ())


-- the compiler
-- Q: why this could not be implemented with just a datatype, why GADT?

-- Q: What is HOAS?


data ByteCode t where
    Var :: Int -> ByteCode t                -- variables identified by numbers
    Lam :: Int -> ByteCode t2 -> ByteCode (t1->t2) -- maybe this Y
    App :: ByteCode (t1->t2) -> ByteCode t1  -> ByteCode t2
    Fix :: Int -> ByteCode t -> ByteCode t
    INT :: Int -> ByteCode Int
    BOOL:: Bool -> ByteCode Bool
    Add :: ByteCode Int -> ByteCode Int -> ByteCode Int
    Mul :: ByteCode Int -> ByteCode Int -> ByteCode Int
    Leq :: ByteCode t1 -> ByteCode t1 -> ByteCode Bool
    IF  :: ByteCode Bool -> ByteCode t -> ByteCode t -> ByteCode t
    LIFT :: t -> ByteCode t                 -- Used only for eval and fmap


instance Show (ByteCode t) where
    show (Var n) = "V" ++ show n
    show (Lam n b) = "(\\V" ++ show n ++ " -> " ++ show b ++ ")"
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Fix n b) = "(fix\\V" ++ show n ++ " " ++ show b ++ ")"
    show (INT n) = show n
    show (BOOL b) = show b
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    show (Leq e1 e2) = "(" ++ show e1 ++ " <= " ++ show e2 ++ ")"
    show (IF be et ee)
        = "(if " ++ show be ++
          " then " ++ show et ++ " else " ++ show ee ++ ")"

newtype C t = C (Int -> (ByteCode t, Int))
unC (C t) vc0 = t vc0

instance Symantics C where
  int x = C (\vc -> (INT x, vc))
  bool b = C (\vc -> (BOOL b, vc))

  lam f = C(\vc -> let v = vc
                       var = C(\vc -> (Var v, vc))
                       (body, vc') = unC (f var) (succ vc)
                   in (Lam v body, vc'))

  app e1 e2 = C(\vc -> let (e1b,vc1) = unC e1 vc
                           (e2b,vc2) = unC e2 vc1
                       in (App e1b e2b,vc2))

  fix f = C(\vc -> let v = vc
                       var = C(\vc -> (Var v, vc))
                       (body,vc') = unC (f var) (succ vc)
                   in (Fix v body, vc'))

  add e1 e2 = C(\vc -> let (e1b,vc1) = unC e1 vc
                           (e2b,vc2) = unC e2 vc1
                       in (Add e1b e2b,vc2))

  mul e1 e2 = C(\vc -> let (e1b,vc1) = unC e1 vc
                           (e2b,vc2) = unC e2 vc1
                       in (Mul e1b e2b,vc2))

  leq e1 e2 = C(\vc -> let (e1b,vc1) = unC e1 vc
                           (e2b,vc2) = unC e2 vc1
                       in (Leq e1b e2b,vc2))

  if_ be et ee = C(\vc -> let (beb,vc1) = unC be vc
                              (etb,vc2)  = unC et vc1
                              (eeb,vc3)  = unC ee vc2
                       in (IF beb etb eeb,vc3))


compC repr = fst $ unC repr 0

test1' () = add (int 1) (int 2)

ctest1 = compC . test1' $ ()

------------------------------------------------------------------------------- ---
--- The partial Evaluator , combination of interpreter (R) and compiler (C)  ---- ---
------------------------------------------------------------------------------------

data P1 t = P1 (Maybe (R t)) (C t)

abstr1 :: P1 t -> C t
abstr1 (P1 _ dyn) = dyn -- extract future stage




-- stumbles on higher-order functions
instance Symantics P1 where
    int  x = P1 (Just (int x)) (int x)
    bool b = P1 (Just (bool b)) (bool b)
    add (P1 (Just n1) _) (P1 (Just n2) _) = int (unR (add n1 n2))
    add e1 e2 = P1 Nothing (add (abstr1 e1) (abstr1 e2))
    mul (P1 (Just n1) _) (P1 (Just n2) _) = int (unR (mul n1 n2))
    mul e1 e2 = P1 Nothing (mul (abstr1 e1) (abstr1 e2))
    leq (P1 (Just n1) _) (P1 (Just n2) _) = bool (unR (leq n1 n2))
    leq e1 e2 = P1 Nothing (leq (abstr1 e1) (abstr1 e2))
    if_ (P1 (Just s) _) et ef = if unR s then et else ef
    if_ eb et ef = P1 Nothing (if_ (abstr1 eb) (abstr1 et) (abstr1 ef))
    lam = undefined
    app = undefined
    fix = undefined
-- Q: Y it impossilbe to implement higher-order functions above?

---------------------------------------------------------------------------
---------------------------------------------------------------------------
--- typecase pattern (FIXME: typecase ?) ----------------------------------
-- Q: what is typecase pattern?

-- FIXME: below is not really understood ...
data P t where
    VI :: Int -> P Int                -- Possibly static (base type)
    VB :: Bool -> P Bool
    VF :: (P a -> P b) -> P (a->b)
    E  :: C t -> P t


abstr :: P t -> C t
abstr (VI i) = int i
abstr (VB b) = bool b
abstr (VF f) = lam (abstr . f . E)
abstr (E x) = x

{-
instance Functor P where
    fmap f = E . fmap f . abstr
-}

-- does mathematic tries to stip off details to understand problem
-- and programmer builds more objects that mitigate the problem?

instance Symantics P where
    int x  = VI x
    bool b = VB b

    -- lam :: (repr a -> repr b) -> repr (a->b)
    lam = VF
    -- app :: repr (a->b) -> repr a -> repr b
    app (VF f) ea = f ea
    app (E f)  ea = E (app f (abstr ea))

    -- fix :: (repr a -> repr a) -> repr a
    {- use to:
    -- For now, to avoid divergence at the PE stage, we residualize
    -- actually, we unroll the fixpoint exactly once, and then
    -- residualize
    fix f = f (E (fix (abstr . f . E)))
    -}
    -- Now, we just go all the way
    -- provided `fixing' produces static results...

    fix f = pfix f -- need this charade for GADTs sake

    add (VI 0) e = e
    add e (VI 0) = e
    add (VI n1) (VI n2) = VI (n1 + n2)
    add e1 e2 = E (add (abstr e1) (abstr e2))
    mul e@(VI 0) _ = e
    mul _ e@(VI 0) = e
    mul (VI 1) e = e
    mul e (VI 1) = e
    mul (VI n1) (VI n2) = VI (n1 * n2)
    mul e1 e2 = E (mul (abstr e1) (abstr e2))

    leq (VI n1) (VI n2) = VB (n1 <= n2)
    leq e1 e2 = E (leq (abstr e1) (abstr e2))

    if_ (VB b1) et ee = if b1 then et else ee
    if_ be      et ee = E (if_ (abstr be) (abstr et) (abstr ee))


-- we need this signature to bind 'a'. That's why it can't be a method
pfix :: forall a. (P a -> P a) -> P a
pfix f = res where
 res:: P a
 res = case f res  of
        E _  -> E (fix (abstr . f . E))
        VF g -> VF (\x ->
                     case x of
                             E cde -> E (app (fix (abstr . f . E)) cde)
                             x     -> g x)
