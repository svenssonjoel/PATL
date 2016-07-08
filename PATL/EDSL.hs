{-# LANGUAGE GADTs #-}



module PATL.EDSL where

import PATL.Shape


data Array sh a 


data Exp a where
  Generate :: Shape (Exp Int)
           -> (Index (Exp Int) -> Exp a)
           -> Exp (Array (Shape (Exp Int)) a)
                   



-- Front-end embedded language for generating PATL.AST
-- TODO: define front-end functions
-- TODO: Detect sharing and establish Lets



generate :: Shape (Exp Int)
         -> (Index (Exp Int) -> Exp a)
         -> Exp (Array (Shape (Exp Int)) a)
generate = Generate 



