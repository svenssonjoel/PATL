{-# LANGUAGE TypeFamilies #-}

module PATL.EDSL.Compile where

import PATL.EDSL.Shape hiding (IIndex, IRange) 

import qualified PATL.AST as A 
import qualified PATL.EDSL as E
import PATL.EDSL.Syntax 


import Data.Reify
import Data.Reify.Graph

import qualified Data.Map as M
import qualified Data.Set as Set


-- This is the end goal
--compile :: E.Exp a -> A.Exp
--compile = undefined 

-- TODO: Detect sharing and establish Lets

-- ------------------------------------------------------------
-- Reify Graph 
-- ------------------------------------------------------------

instance MuRef Expr where
  type DeRef Expr = Syntax
  mapDeRef f (Expr s) = traverse f s


genGraph :: MuRef s => s -> IO (Graph (DeRef s))
genGraph = reifyGraph 


-- ------------------------------------------------------------
--  
-- ------------------------------------------------------------


graphToAST :: Graph Syntax -> Maybe A.Exp
graphToAST (Graph edges root) = undefined 

    

   
