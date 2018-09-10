module  ApplicativeConst.MyConst
    (
    ) where


-- Wow https://blog.jle.im/entry/const-applicative-and-monoids.html

newtype Const w a = Const { getConst :: w }

newtype IntConst a = IntConst { getIntConst :: Int }

instance Functor (Const w) where
  fmap _ (Const w) = Const w


instance Functor IntConst where
  fmap _ (IntConst w) = IntConst w

{-
fmap1 :: (a -> b     ) -> F a -> F b
fmap2 :: (a -> b -> c) -> F a -> F b -> F c
-}

instance Applicative IntConst where
  pure x = IntConst 0
  (IntConst w1) <*> (IntConst w2) = IntConst (w1 + w2)

instance Monoid w => Applicative (Const w) where
    pure _              = Const mempty
    Const x <*> Const y = Const (x `mappend` y)
