{-# LANGUAGE GADTs #-}



module PATL.EDSL where

import PATL.Value
import PATL.Operators 
import PATL.EDSL.Shape
import PATL.EDSL.Syntax hiding (IRange, IIndex) 
import qualified PATL.EDSL.Syntax as S 

import Prelude hiding (map, zipWith, div ) 
import qualified Prelude as P



-- TODO: Shape is part of Array type..
--       But is not conveying any information!
--       This is because the actual shape is not encoded in the type.
--       Either have the "actual" shape in the type or remove shape
--       from array types (as it is stating the obvious -- this array has a shape).
--      
data Array sh a 




-- Front-end embedded language for generating PATL.AST
-- TODO: define front-end functions
-- TODO: Detect sharing and establish Lets


-- Testing: 
generate :: Exp (Shape (Exp Int))
         -> Exp (Index (Exp Int) -> Exp a)
         -> Exp (Array (Shape (Exp Int)) a)
generate sh f =  liftSE $ Generate (toExp sh) (toExp f) 
                                  
map :: Exp (Exp a -> Exp b) 
    -> Exp (Array (Shape (Exp Int)) (Exp a)) 
    -> Exp (Array (Shape (Exp Int)) (Exp b))
map f arr = liftSE $ Map (toExp f) (toExp arr)

zipWith :: Exp (Exp a -> Exp b -> Exp c) 
        -> Exp (Array (Shape (Exp Int)) (Exp a))
        -> Exp (Array (Shape (Exp Int)) (Exp b))
        -> Exp (Array (Shape (Exp Int)) (Exp c))
zipWith f a1 a2 = liftSE $ ZipWith (toExp f) (toExp a1) (toExp a2) 

-- Reduce all the way to scalar 
reduce :: Exp (Exp a -> Exp b -> Exp b)
       -> Exp b
       -> Exp (Array (Shape (Exp Int)) (Exp a))
       -> Exp b
reduce f b arr = liftSE $ Reduce (toExp f) (toExp b) (toExp arr) 

-- Create an array 
iota :: Exp (Shape (Exp Int))
     -> Exp (Array (Shape (Exp Int)) (Exp Int))
iota sh = liftSE $ Iota (toExp sh) 


-- TODO: Project becomes tricky at this point
--prj :: (Exp (Index (Exp Int)))
--    -> Exp (Array (Shape (Exp Int)) (Exp a))
--    -> 


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
         
        
-- ------------------------------------------------------------
-- Num 
-- ------------------------------------------------------------

liftSE :: Syntax Expr -> Exp a
liftSE = Exp . Expr 

instance Num a => Num (Exp a) where
  (+) a b = liftSE $ Op Add [toExp a, toExp b]
  (-) a b = liftSE $ Op Sub [toExp a, toExp b]
  (*) a b = liftSE $ Op Mul [toExp a, toExp b]

  abs = undefined
  signum = undefined

  fromInteger i = liftSE $ Constant (VInt (fromInteger i))  



-- Integer division
div :: Integral a => Exp a -> Exp a -> Exp a
div a b = liftSE $ Op Div [toExp a, toExp b] 
