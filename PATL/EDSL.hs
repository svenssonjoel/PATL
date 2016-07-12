{-# LANGUAGE GADTs #-}



module PATL.EDSL where

import PATL.Value
import PATL.Shape
import PATL.EDSLSyntax hiding (IRange, IIndex) 
import qualified PATL.EDSLSyntax as S 

import Prelude hiding (map) 
import qualified Prelude as P



data Array sh a 




-- Front-end embedded language for generating PATL.AST
-- TODO: define front-end functions
-- TODO: Detect sharing and establish Lets


-- Testing: 
generate :: Exp (Shape (Exp Int))
         -> Exp (Index (Exp Int) -> Exp a)
         -> Exp (Array (Shape (Exp Int)) a)
generate sh f =  Exp $ Expr $ Generate (Expr (Constant (VInt 1)))
                                       (Expr (Constant (VInt 1)))






-- ------------------------------------------------------------
-- 
-- ------------------------------------------------------------
class Expable a where
  toExp :: a -> Expr

instance Expable (Exp a) where
  toExp = unExp

instance (Expable a, Expable b) => Expable (a -> b) where
  toExp = undefined

--Shape is now entirely a front end thing
instance Expable a => Expable (Shape a) where
  toExp = undefined

instance Expable a => Expable (I a) where
  toExp = undefined 

--instance Expable a => Expable (Index a) where
--  toExp = undefined 
         
        
