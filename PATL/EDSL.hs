{-# LANGUAGE GADTs, TypeOperators, FlexibleInstances, FlexibleContexts  #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module PATL.EDSL where

import PATL.Value
import PATL.Operators
import PATL.TuneParam
import PATL.EDSL.Shape
import PATL.EDSL.Syntax hiding (IRange, IIndex, Z, IAll ) 
import qualified PATL.EDSL.Syntax as S 

import Prelude hiding (map, zipWith, div ) 
import qualified Prelude as P


-- Type level tag to annotate Array valued expressions 
data Array (sh :: [*]) a

-- ------------------------------------------------------------
-- Front-end embedded language for generating PATL.AST
-- ------------------------------------------------------------

generate :: Exp (Shape sh)
         -> Exp (Index sh -> Exp a)
         -> Exp (Array sh a)
generate sh f =  liftSE $ Generate (toExp sh) (toExp f) 
                                  
map :: Exp (Exp a -> Exp b) 
    -> Exp (Array sh (Exp a)) 
    -> Exp (Array sh (Exp b))
map f arr = liftSE $ Map (toExp f) (toExp arr)

zipWith :: Exp (Exp a -> Exp b -> Exp c) 
        -> Exp (Array sh (Exp a))
        -> Exp (Array sh (Exp b))
        -> Exp (Array sh (Exp c))
zipWith f a1 a2 = liftSE $ ZipWith (toExp f) (toExp a1) (toExp a2) 

-- Reduce all the way to scalar 
reduce :: Exp (Exp a -> Exp b -> Exp b)
       -> Exp b
       -> Exp (Array sh (Exp a))
       -> Exp b
reduce f b arr = liftSE $ Reduce (toExp f) (toExp b) (toExp arr) 

-- Create an array 
iota :: Exp (Shape sh)
     -> Exp (Array sh (Exp Int))
iota sh = liftSE $ Iota (toExp sh) 


-- A tuning parameter of type Int
-- TODO: Make sure we can detect sharing of these!
--       I suspect they may always be inlined 
tInt :: Exp Int 
tInt = liftSE $ TuneParam TPInt 

-- Extract row and column from 2d array 
extract_row :: Exp (Array '[Exp Int,Exp Int] (Exp a))
            -> Exp Int
            -> Exp (Array '[Exp Int] (Exp a))
extract_row arr row = liftSE
                      $ Prj (toExp arr)
                            (toExp (IIndex row:.IAll:.Z
                                    :: Shape '[I (Exp Int),I (Exp Int)]))
                                       -- Need to annotate here
                                       -- to be able to find the toExp instance

extract_col :: Exp (Array '[Exp Int,Exp Int] (Exp a))
            -> Exp Int
            -> Exp (Array '[Exp Int] (Exp a))
extract_col arr col = liftSE
                      $ Prj (toExp arr)
                            (toExp (IAll:.IIndex col:.Z 
                                    :: Shape '[I (Exp Int),I (Exp Int)]))
                                       -- Need to annotate here
                                       -- to be able to find the toExp instance
                                       
extract_page :: Exp (Array '[Exp Int, Exp Int, Exp Int] (Exp a))
             -> Exp Int
             -> Exp (Array '[Exp Int, Exp Int] (Exp a))
extract_page arr page = liftSE
                        $ Prj (toExp arr)
                              (toExp (IIndex page:.IAll:.IAll:.Z)
                                      :: Shape '[I (Exp Int), I (Exp Int), I (Exp Int)])
                     
                                       
                                       

-- Index all the way to a scalar
-- TODO: Enforce ix is an index of shape sh.
--       Is the below attempt ok ?                                     
index :: Exp (Array sh (Exp a))
      -> Exp (Index sh)
      -> Exp a
index arr ix = liftSE $ Prj (toExp arr)
                            (toExp ix)
                                       
                                       
-- TODO: prj becomes tricky at this point
               
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
instance Expable (Shape '[]) where
  toExp Z =  Expr S.Z

instance (Expable (Shape b), Expable a) => Expable (Shape (a ': b)) where
  toExp (a:.b) = Expr $ S.Cons (toExp a) (toExp b) 
         

instance Expable a => Expable (I a) where
  toExp (IIndex i)   = Expr $ S.IIndex (toExp i)
  toExp (IRange i j) = Expr $ S.IRange (toExp i) (toExp j)
  toExp  IAll        = Expr $ S.IAll 
  
         
        
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
