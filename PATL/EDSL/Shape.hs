{-# LANGUAGE TypeOperators, EmptyDataDecls, GADTs #-}

module PATL.EDSL.Shape where 

{- Shapes, extents and indexing -}

data a :. b
data Z 

-- TODO: Do we want shapes in the types in the embedding
data Shape a where
  Z :: Shape Z
  (:.) :: (Shape b) -> a -> Shape (b :. a)

--data Shape a = Z
--             | (Shape a) :. a 

data I a = IIndex a
         | IRange a a
         | IAll 


type Index a = Shape (I a) 
