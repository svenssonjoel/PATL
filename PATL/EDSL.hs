{-# LANGUAGE GADTs #-}



module PATL.EDSL where

import PATL.Value
import PATL.Shape
import PATL.EDSLSyntax

import Prelude hiding (map) 
import qualified Prelude as P 


data Array sh a 


newtype Exp a = Exp {unExp :: Expr}

newtype Expr = Expr {syntax :: Syntax Expr}
                 deriving (Show) 


-- Front-end embedded language for generating PATL.AST
-- TODO: define front-end functions
-- TODO: Detect sharing and establish Lets


-- Testing: 
generate :: Exp (Shape (Exp Int))
         -> (Exp (Index (Exp Int)) -> Exp a)
         -> Exp (Array (Shape (Exp Int)) a)
generate sh f =  Exp $ Expr $ Generate (Expr (Constant (VInt 1)))
                                       (Expr (Constant (VInt 1)))





-- ------------------------------------------------------------
-- Fold Expr
-- ------------------------------------------------------------

foldExpr :: (a -> Expr -> a) -> a -> Expr -> a
foldExpr f a (Expr e) = fse (\i j -> f i (Expr j))  a e 
  where
    fse :: (a -> Syntax Expr -> a) -> a -> Syntax Expr -> a
    fse f a e@(Sh (shp)) = fse f a (syntax shp)
    fse f a e@(Ix (idx)) = fse f a (syntax idx)
    fse f a e@(Op _ es)  = f (foldl f a (P.map syntax es)) e
    fse f a e@(Lam id s) = f (f a (syntax s)) e 
    fse f a e = f a e 

