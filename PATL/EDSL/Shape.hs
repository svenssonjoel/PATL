{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module PATL.Shape where 

{- Shapes, extents and indexing -} 

-- TODO: Do we want shapes in the types in the embedding
data Shape a = Z
             | (Shape a) :. a
             deriving (Functor, Foldable, Traversable, Eq,Show)

data I a = IIndex a
         | IRange a a
         | IAll 
         deriving (Functor, Foldable, Traversable, Eq,Show)


type Index a = Shape (I a) 
