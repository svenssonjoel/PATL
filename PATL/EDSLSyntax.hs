
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}


module PATL.EDSLSyntax where 


import PATL.Shape
import PATL.TuneParam 
import PATL.Value
import PATL.Operators

type Identifier = String 

data Syntax s = Constant Value
              | Var Identifier 
              | TuneParam TP
              | Op Op [s]
              | Lam Identifier s
              | App s s
              | Iota (Shape s)
              | Prj s (Index s)
              | SizeOf s
              | Generate (Shape s) s
              | Map s s
              | ZipWith s s s
              | Reduce s s s
              deriving (Functor, Foldable, Traversable, Show)
              
              

