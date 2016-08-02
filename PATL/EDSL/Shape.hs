{-# LANGUAGE TypeOperators, GADTs #-}
{-# LANGUAGE DataKinds #-}

module PATL.EDSL.Shape where 

import Data.Functor.Identity 

-- Shapes are represented by (type level) lists

data Shape_ f a where
  Z :: Shape_ f '[]
  (:.) :: f a -> Shape_ f b -> Shape_ f (a ': b)


type Shape a = Shape_ Identity a 
type Index a = Shape_ I a 

infixr 5 :.

data I a = IIndex a
         | IRange a a
         | IAll 


-- newtype Index a = Index (Shape a)
