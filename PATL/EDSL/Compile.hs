{-# LANGUAGE TypeFamilies #-}

module PATL.EDSL.Compile where

import PATL.EDSL.Shape hiding (IIndex, IRange) 

import qualified PATL.AST as A 
import qualified PATL.EDSL as E
import PATL.EDSL.Syntax 


import Data.Reify
import Data.Reify.Graph

import qualified Data.Map as M
-- import qualified Data.Set as Set

import Control.Monad.State


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

type G a = State (M.Map Unique A.Exp) a 

graphToAST :: Graph Syntax -> Maybe A.Exp
graphToAST (Graph edges root) = evalState ( doIt root ) M.empty 
  where
    doIt ::  Unique -> G (Maybe A.Exp)
    doIt nid =
      case lookup nid edges of -- currently list lookup 
        Nothing -> return Nothing
        Just node ->
          case node of
            -- A constant, just convert it to AST  
            (Constant v) ->
                 let ast_node = A.Constant v
                 in return $ Just ast_node
            -- These variables can only come from Lambdas in the EDSL!
            (Var (FunArg i)) -> return $ Just (A.Var ("l" ++ show i))
            (TuneParam tp)   ->
              do bound <- get
                 -- look it up and return if found. otherwise return self
                 case M.lookup nid bound of
                   Nothing -> return $ Just (A.TuneParam tp)
                   Just v  -> return $ Just v

            -- Introduce let bindings for "ss" (and treat recursively) 
            (Tuple ss) ->
              do bound <- get
                 
                 let vars = [A.Var ("v" ++ show i) | i <- ss]
                     done = map (\nodid -> M.lookup nodid bound) ss :: [Maybe A.Exp]
                     dv   = zip done ss -- indicates which should be introduced here
                 fmap Just $ bindMissing dv (A.Tuple) 
                 
                 
    bindMissing :: [(Maybe A.Exp, Unique)] -> ([A.Exp] -> A.Exp) -> G A.Exp 
    bindMissing xs f = do (acc,f') <- doLet xs [] f
                          return $ f' (reverse acc)
      where
        doLet ((m,u):xs) acc f
          | m == Nothing =
            do let v = (A.Var ("v" ++ show u))
               e <- doIt u
               case e of
                 Nothing -> error "CLEANUP" 
                 Just e' -> do
                   bound <- get
                   put (M.insert u v bound)
                   return (v:acc, \vars -> A.Let ("v" ++ show u) e' (f vars))
                                  
                
      
  


    

   
