
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}


module PATL.EDSL.Syntax where 


-- import PATL.Shape
import PATL.TuneParam 
import PATL.Value
import PATL.Operators

import Prelude hiding (map) 
import qualified Prelude as P

import qualified Data.Set as Set 

type Identifier = Integer

data Syntax s = Constant Value
              | Var Identifier 
              | TuneParam TP
              | Sh s
              | Ix s
              | Z | IAll | IIndex s | IRange s s | Snoc s s 
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



              
newtype Exp a = Exp {unExp :: Expr}

newtype Expr = Expr {syntax :: Syntax Expr}
                 deriving (Show) 
              

-- ------------------------------------------------------------
-- Fold Expr
-- ------------------------------------------------------------

foldExpr :: (a -> Expr -> a) -> a -> Expr -> a
foldExpr f a (Expr e) = fse (\i j -> f i (Expr j))  a e 
  where
    fse :: (a -> Syntax Expr -> a) -> a -> Syntax Expr -> a
    fse f a e@(Sh (shp)) = fse f a (syntax shp)
    fse f a e@(Ix (idx)) = fse f a (syntax idx)
    fse f a e@(IIndex s) = f (f a (syntax s)) e 
    fse f a e@(IRange s1 s2) = f (foldl f a (P.map syntax [s1,s2])) e
    fse f a e@(Op _ es)  = f (foldl f a (P.map syntax es)) e
    fse f a e@(Lam id s) = f (f a (syntax s)) e
    fse f a e@(App s1 s2) = f (foldl f a (P.map syntax [s1,s2])) e
    fse f a e@(Iota s)  = f (f a (syntax s)) e
    fse f a e@(Prj s1 s2) = f (foldl f a (P.map syntax [s1,s2])) e
    fse f a e@(SizeOf s) = f (f a (syntax s)) e
    fse f a e@(Generate s1 s2) = f (foldl f a (P.map syntax [s1,s2])) e
    fse f a e@(Map s1 s2) = f (foldl f a (P.map syntax [s1,s2])) e
    fse f a e@(ZipWith s1 s2 s3) = f (foldl f a (P.map syntax [s1,s2,s3])) e
    fse f a e@(Reduce s1 s2 s3) = f (foldl f a (P.map syntax [s1,s2,s3])) e
    fse f a e = f a e -- nonrecursive cases 

-- Collect identifier 
idCollector :: Syntax Expr -> Set.Set Identifier
idCollector s = doIt Set.empty s
  where
    doIt set (Var id) =  Set.insert id set
    doIt set (Lam id _) = Set.insert id set
    doIt set _ = set 
