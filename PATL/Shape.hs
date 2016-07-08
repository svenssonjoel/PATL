
module PATL.Shape where 

{- Shapes, extents and indexing -} 

data Shape a = Z
             | (Shape a) :. a
             deriving (Eq,Show)

data I a = IIndex a
         | IRange a a
         | IAll 
         deriving (Eq,Show)


type Index a = Shape (I a) 
