{-# LANGUAGE GADTs, TypeOperators, FlexibleInstances, FlexibleContexts  #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}      --- Which of these are really needed?

module PATL.EDSL where

import PATL.Value
import PATL.Operators
import PATL.TuneParam
import PATL.EDSL.Shape
import PATL.EDSL.Syntax hiding (IRange, IIndex, Z, IAll ) 
import qualified PATL.EDSL.Syntax as S 

import Prelude hiding (map, zipWith, div ) 
import qualified Prelude as P

import qualified Data.Set as Set


-- Type level tag to annotate Array valued expressions 
data Array (sh :: [*]) a

-- ------------------------------------------------------------
-- Front-end embedded language for generating PATL.AST
-- ------------------------------------------------------------

generate :: Exp (Shape sh)
         -> Exp (Exp (Index sh) -> Exp a)
         -> Exp (Array sh (Exp a))
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
tInt :: Int -> Int -> Exp Int 
tInt i j = liftSE $ TuneParam (TPIntRange i j)

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
                              (toExp (IIndex page:.IAll:.IAll:.Z
                                      :: Shape '[I (Exp Int), I (Exp Int), I (Exp Int)]))
                     
                                       
                                       

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

instance Expable (Exp a -> Exp b) where
  toExp f =
    let (Exp e) = f $ var (-1)
        the_id = Set.findMax (Set.insert (-1) $ collectIds e) + 1
        (Exp e_real) = f $ var the_id
        in Expr $ Lam the_id e_real

instance Expable (Exp a -> Exp b -> Exp c) where
  toExp f =
    let (Exp e) = f (var (-1)) (var (-2)) 
        the_id1 = Set.findMax
                   (Set.insert (-1) $ Set.insert (-2) $ collectIds e) + 1
        the_id2 = the_id1 + 1
        

        (Exp e_real) = f (var the_id1) (var the_id2) 
        in Expr $ Lam the_id1 $ Expr $ Lam the_id2 e_real


    

--Shape is now entirely a front end thing
instance Expable (Shape '[]) where
  toExp Z =  Expr S.ShapeZ

instance (Expable (Shape b), Expable a) => Expable (Shape (a ': b)) where
  toExp (a:.b) = Expr $ S.ShapeCons (toExp a) (toExp b) 
         

instance Expable a => Expable (I a) where
  toExp (IIndex i)   = Expr $ S.IIndex (toExp i)
  toExp (IRange i j) = Expr $ S.IRange (toExp i) (toExp j)
  toExp  IAll        = Expr $ S.IAll


instance (Expable a, Expable b) => Expable (a,b) where
  toExp (a,b) = Expr $ Tuple [toExp a, toExp b]

instance (Expable a, Expable b, Expable c) => Expable (a,b,c) where
  toExp (a,b,c) = Expr $ Tuple [toExp a, toExp b, toExp c]

instance Expable Int where
  toExp i = Expr $ Constant (VInt i)

instance Expable Float where
  toExp f = Expr $ Constant (VFloat f)

-- Need to "embed", for example, haskell functions on expressions
-- before using them as argument to a pattern.
emb :: Expable a => a -> Exp a 
emb a = Exp . toExp $ a 

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
