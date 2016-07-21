{-# LANGUAGE TypeOperators, GADTs #-}
{-# LANGUAGE DataKinds #-}

module PATL.EDSL.Shape where 

-- Shapes are represented by (type level) lists

data Shape a where
  Z :: Shape '[]
  (:.) :: a -> Shape b -> Shape (a ': b)

infixr 5 :.

data I a = IIndex a
         | IRange a a
         | IAll 


newtype Index a = Index (Shape a)
