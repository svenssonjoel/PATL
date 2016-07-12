{-# LANGUAGE TypeOperators, EmptyDataDecls, GADTs #-}
{-# LANGUAGE DataKinds #-}

module PATL.EDSL.Shape where 

-- Shapes are represented by (type level) lists

-- TODO: Do we want shapes in the types in the embedding
data Shape a where
  Z :: Shape '[]
  (:.) :: (Shape b) -> a -> Shape (a ': b)

--data Shape a = Z
--             | (Shape a) :. a 

data I a = IIndex a
         | IRange a a
         | IAll 


type Index a = Shape a
