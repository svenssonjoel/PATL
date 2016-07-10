
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}


module PATL.EDSLSyntax where 


import PATL.Shape
import PATL.TuneParam 
import PATL.Value
import PATL.Operators

type Identifier = Integer

data Syntax s = Constant Value
              | Var Identifier 
              | TuneParam TP
              | Sh (Shape s)
              | Ix (Index s) 
              | Op Op [s]
              | Lam Identifier s
              | App s s
              | Iota s -- (Shape s)
              | Prj s s -- (Index s)
              | SizeOf s
              | Generate s s -- (Shape s) s
              | Map s s
              | ZipWith s s s
              | Reduce s s s
              deriving (Functor, Foldable, Traversable, Show)
              
              

