{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module PATL.Shape where 

{- Shapes, extents and indexing -} 

data Shape a = Z
             | (Shape a) :. a
             deriving (Functor, Foldable, Traversable, Eq,Show)

data I a = IIndex a
         | IRange a a
         | IAll 
         deriving (Functor, Foldable, Traversable, Eq,Show)


type Index a = Shape (I a) 
