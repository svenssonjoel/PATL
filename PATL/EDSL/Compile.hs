{-# LANGUAGE TypeFamilies #-}

module PATL.EDSL.Compile where

import PATL.EDSL.Shape hiding (IIndex, IRange, Z ) 

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
            -- A constant, just convert it to AST.
            (Constant v) -> do 
                 let ast_node = A.Constant v
              --   bound <- get
              --   put (M.insert nid ast_node bound)
                 return $ Just ast_node
            (Z) -> return $ Just A.Z
            -- These variables can only come from Lambdas in the EDSL!
            (Var (FunArg i)) -> do
              let ast_node = A.Var ("a" ++ show i) 
            --  bound <- get
            --  put (M.insert nid ast_node bound)
              return $ Just ast_node 
            (TuneParam tp)   ->
              do bound <- get
                 -- look it up and return if found. otherwise return self
                 case M.lookup nid bound of
                   Nothing -> return $ Just (A.TuneParam tp)
                   Just v  -> return $ Just v

            -- Introduce let bindings for "ss" (and treat recursively) 
            (Tuple ss) ->
              do bound <- get               
                 let done = map (\nodid -> M.lookup nodid bound) ss :: [Maybe A.Exp]
                     dv   = zip done ss -- indicates which should be introduced here
                 fmap Just $ bindMissing dv (A.Tuple)
            (Op op ss) -> 
              do bound <- get               
                 let done = map (\nodid -> M.lookup nodid bound) ss :: [Maybe A.Exp]
                     dv   = zip done ss -- indicates which should be introduced here
                 fmap Just $ bindMissing dv (A.Op op)

            -- Lambdas probably need special treatment!
            (Lam (FunArg a) e) ->
              do e' <- doIt e
                 case e' of
                   Nothing -> return Nothing
                   Just body ->           
                     return $ Just (A.Lam ("a" ++ show a) body )

            -- Application 
            (App s1 s2) -> 
              do bound <- get               
                 let done = map (\nodid -> M.lookup nodid bound) [s1,s2] :: [Maybe A.Exp]
                     dv   = zip done [s1,s2] -- indicates which should be introduced here            
                 fmap Just $ bindMissing dv (\[e1,e2] -> A.App e1 e2)
                   -- pattern match here will lead to ugly error if broken
            (Map s1 s2) -> 
              do bound <- get               
                 let done = map (\nodid -> M.lookup nodid bound) [s1,s2] :: [Maybe A.Exp]
                     dv   = zip done [s1,s2] -- indicates which should be introduced here            
                 fmap Just $ bindMissing dv (\[e1,e2] -> A.Map e1 e2)
                   -- pattern match here will lead to ugly error if broken
            (Generate s1 s2) -> 
              do bound <- get               
                 let done = map (\nodid -> M.lookup nodid bound) [s1,s2] :: [Maybe A.Exp]
                     dv   = zip done [s1,s2] -- indicates which should be introduced here            
                 fmap Just $ bindMissing dv (\[e1,e2] -> A.Generate e1 e2)
                   -- pattern match here will lead to ugly error if broken

            (Reduce s1 s2 s3) -> 
              do bound <- get               
                 let done = map (\nodid -> M.lookup nodid bound) [s1,s2,s3] :: [Maybe A.Exp]
                     dv   = zip done [s1,s2,s3] -- indicates which should be introduced here            
                 fmap Just $ bindMissing dv (\[e1,e2,e3] -> A.Reduce e1 e2 e3)
                   -- pattern match here will lead to ugly error if broken
            (ZipWith s1 s2 s3) -> 
              do bound <- get               
                 let done = map (\nodid -> M.lookup nodid bound) [s1,s2,s3] :: [Maybe A.Exp]
                     dv   = zip done [s1,s2,s3] -- indicates which should be introduced here            
                 fmap Just $ bindMissing dv (\[e1,e2,e3] -> A.ZipWith e1 e2 e3)
                   -- pattern match here will lead to ugly error if broken
            (Iota s) -> 
              do bound <- get               
                 let done = map (\nodid -> M.lookup nodid bound) [s] :: [Maybe A.Exp]
                     dv   = zip done [s] -- indicates which should be introduced here            
                 fmap Just $ bindMissing dv (\[e] -> A.Iota e)
                   -- pattern match here will lead to ugly error if broken
            (Cons s1 s2) -> 
              do bound <- get               
                 let done = map (\nodid -> M.lookup nodid bound) [s1,s2] :: [Maybe A.Exp]
                     dv   = zip done [s1,s2] -- indicates which should be introduced here            
                 fmap Just $ bindMissing dv (\[e1,e2] -> A.Cons e1 e2)
                   -- pattern match here will lead to ugly error if broken
            a -> error (show a)
                 
    bindMissing :: [(Maybe A.Exp, Unique)] -> ([A.Exp] -> A.Exp) -> G A.Exp 
    bindMissing xs f = do (acc,f') <- doLet xs [] f
                          return $ f' (reverse acc)
      where
        doLet [] acc f = return (acc,f)
        doLet ((m,u):xs) acc f = 
          case m of
          Nothing -> 
            do let v = (A.Var ("v" ++ show u))
               e <- doIt u
               case e of
                 Nothing -> error "CLEANUP" 
                 Just e' -> do
                   case shouldLet e' of
                     False -> doLet xs (e':acc) f
                     True -> do 
                       bound <- get
                       put (M.insert u v bound)
                       doLet xs
                             (v:acc)
                             (\vars -> A.Let ("v" ++ show u) e' (f vars)) 
          -- In case a binding already exists.
          Just e ->
            doLet xs (e:acc) f

        --refine this 
        shouldLet (A.Constant _)  = False
        shouldLet (A.Var _)       = False
        shouldLet (A.Z)           = False
        shouldLet (A.TuneParam _) = False
        shouldLet (A.IAll)        = False
        shouldLet (A.Cons _ _)    = False
        shouldLet a               = True 

  


    

   
