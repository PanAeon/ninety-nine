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

-- λx.power x 7
testpowfix7 () = lam (\x ->
            app ( app (testpowfix () ) x)  (int 7))

-- The dummy argument () above is to avoid the monomorphism restriction,
