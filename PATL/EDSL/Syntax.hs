
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 

module PATL.EDSL.Syntax where 


-- import PATL.Shape
import PATL.TuneParam 
import PATL.Value
import PATL.Operators

import Prelude hiding (map) 
import qualified Prelude as P

import qualified Data.Set as Set 

newtype Identifier = FunArg Integer
                     deriving (Num, Eq, Ord, Show)

data Syntax s = Constant Value
              | Var Identifier 
              | TuneParam TP
              | Tuple [s]
              | Sh s
              | Ix s
                -- I dont like this duplication. 
              | ShapeZ | IndexZ | IAll | IIndex s | IRange s s | ShapeCons s s | IndexCons s s 
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

var = Exp . Expr . Var 
              

-- ------------------------------------------------------------
-- Fold Expr
-- ------------------------------------------------------------

foldExpr :: (a -> Expr -> a) -> a -> Expr -> a
foldExpr f a (Expr e) = fse (\i j -> f i (Expr j))  a e 
  where
    fse :: (a -> Syntax Expr -> a) -> a -> Syntax Expr -> a
    fse f a e@(Tuple es) = f (foldl (fse f) a (P.map syntax es)) e
    fse f a e@(Sh (shp)) = f (fse f a (syntax shp)) e 
    fse f a e@(Ix (idx)) = f (fse f a (syntax idx)) e
    fse f a e@(IIndex s) = f ((fse f) a (syntax s)) e 
    fse f a e@(IRange s1 s2) = f (foldl (fse f) a (P.map syntax [s1,s2])) e
    fse f a e@(Op _ es)  = f (foldl (fse f) a (P.map syntax es)) e
    fse f a e@(Lam id s) = f ((fse f) a (syntax s)) e
    fse f a e@(App s1 s2) = f (foldl (fse f) a (P.map syntax [s1,s2])) e
    fse f a e@(Iota s)  = f ((fse f) a (syntax s)) e
    fse f a e@(Prj s1 s2) = f (foldl (fse f) a (P.map syntax [s1,s2])) e
    fse f a e@(SizeOf s) = f ((fse f) a (syntax s)) e
    fse f a e@(Generate s1 s2) = f (foldl (fse f) a (P.map syntax [s1,s2])) e
    fse f a e@(Map s1 s2) = f (foldl (fse f) a (P.map syntax [s1,s2])) e
    fse f a e@(ZipWith s1 s2 s3) = f (foldl (fse f) a (P.map syntax [s1,s2,s3])) e
    fse f a e@(Reduce s1 s2 s3) = f (foldl (fse f) a (P.map syntax [s1,s2,s3])) e
    fse f a e = f a e -- nonrecursive cases 

-- Collect identifier 
idCollector :: Set.Set Identifier -> Expr -> Set.Set Identifier
idCollector a (Expr s) = doIt a s
  where
    doIt set (Var id) =  Set.insert id set
    doIt set (Lam id _) = Set.insert id set
    doIt set _ = set 

-- ------------------------------------------------------------
-- collect identifiers 
-- ------------------------------------------------------------

collectIds :: Expr -> Set.Set Identifier
collectIds e = foldExpr idCollector Set.empty e 
