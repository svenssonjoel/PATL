{-# LANGUAGE TypeFamilies #-}

module PATL.EDSL.Compile where

import PATL.EDSL.Shape

import qualified PATL.AST as A 
import qualified PATL.EDSL as E
import PATL.EDSL.Syntax 


import Data.Reify
import Data.Reify.Graph


-- This is the end goal
--compile :: E.Exp a -> A.Exp
--compile = undefined 


-- ------------------------------------------------------------
-- Reify Graph 
-- ------------------------------------------------------------

instance MuRef Expr where
  type DeRef Expr = Syntax
  mapDeRef f (Expr s) = traverse f s


genGraph :: MuRef s => s -> IO (Graph (DeRef s))
genGraph = reifyGraph 


-- ------------------------------------------------------------
-- Graph Analysis
-- ------------------------------------------------------------


