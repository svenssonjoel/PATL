{-# LANGUAGE GADTs #-}



module PATL.EDSL where

import PATL.Value
import PATL.Shape
import PATL.EDSLSyntax


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



