{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module PATL.Eval where


import PATL.AST
import PATL.Value
import PATL.Operators
import PATL.Patterns
import PATL.TuneParam

import qualified Data.Vector as V 
import qualified Data.Map as M

import Control.Monad.State

import Text.Show.Functions

import Test.QuickCheck 

------------------------------------------------------------
-- Evaluation

type EvalShape = [EvalResult]

-- Clean this up later
data EvalResult = Scalar Value
                | Tup    [EvalResult]
                | Array  EvalResult (V.Vector (EvalResult))
                | Shap   EvalShape  -- Better constr names! (maybe prefix Eval_)
                | Idx    EvalShape
                 
                | Idx_IAll
                | Idx_IIndex EvalResult
                | Idx_IRange EvalResult EvalResult
                | Function (EvalResult -> EvalResult)
                deriving ( Show)

rInt i = Scalar (VInt i) 

instance Eq EvalResult where
  (Scalar v1) == (Scalar v2) = v1 == v2
  (Array s1 v1) == (Array s2 v2) = s1 == s2 && v1 == v2
  (Shap v1) == (Shap v2) = v1 == v2
  (Idx v1)  == (Idx v2)  = v1 == v2
  (Idx_IAll) == (Idx_IAll) = True
  (Idx_IIndex v1) == (Idx_IIndex v2) = v1 == v2
  (Idx_IRange a1 a2) == (Idx_IRange b1 b2) = a1 == b1 && a2 == b2
  (Function f) == (Function g) = error "comparing equality on functions"
  a == b = False
  

-- -------------------------------------------------------
-- Environment
-- -------------------------------------------------------
type Env = M.Map Identifier EvalResult 

emptyEnv = M.empty


-- -------------------------------------------------------
-- Evaluation monad
-- -------------------------------------------------------
type E a = State Env a

-- -------------------------------------------------------
-- Evaluate and expression given an environment
-- -------------------------------------------------------
eval :: Env -> Exp -> EvalResult
eval env e = evalState (doEval e) env 
  where
    doEval :: Exp -> E EvalResult 
    doEval e =
      case e of
      
      Constant v -> return $ Scalar v

      Var ident  ->
        do env <- get 
           case M.lookup ident env of
             Nothing -> error $ "Environment malfunction: " ++ show ident ++ "\n" ++ show env 
             Just v  -> return v

      -- ------------------------------
      -- Tuning parameters should have been
      -- removed before evaluation.
      -- extractTuneParams replaces these with variables.

      TuneParam t -> error $ "Tuning parameter present at eval" 

      -- Evaluate tuples
      Tuple es -> do
        es' <- mapM doEval es 
        return $ Tup es'

      -- Evaluate Lam to haskell functions 
      Lam ident e ->
        do env <- get -- capture environment here 
           
           return $ Function
                  $ \fr -> evalState ( 
             do env <- get
                let env2 = M.insert ident fr env
                put env2 
                res <- doEval e
                return res
             ) env
             -- changes to env within function does not leak out 
                          
      -- evaluate App to Haskell function application 
      App fun e ->
        do efun <- doEval fun
           ee   <- doEval e
           case efun of
             Function f -> return $ f ee
             _          -> appNotAFun

      -- Evaluate any operator
      Op op es -> evalOp op es

      -- SHAPES
      Sh shape -> do
        doEval shape -- evalExtents shape 
        -- return $ Shap ext

      Ix shape -> do
        doEval shape -- evalIndex shape
        --return $ Idx idx

      -- Let bindings. Extends the environment
      Let ident e1 e2 ->
        do ee1 <- doEval e1
           env <- get
           let env2 = M.insert ident ee1 env
           put env2
           doEval e2

      -- Iota. Shape to array 
      Iota extents -> do
        extents' <- doEval extents -- evalExtents extents
        evalIota extents' -- hmm
        
      -- Project out of container
      -- This one is scary
      -- TODO: Implement! 
      Prj e idx -> do
        idx' <- doEval idx
        e' <- doEval e    
        doPrj e' idx'
              
      -- SizeOf can return either a scalar or a shape. 
      SizeOf e ->
        do e' <- doEval e
           case e' of
             Scalar _ -> return $ Scalar (VInt 1)
             Array sh _ -> return $ sh 


      -- PATTERNS
      Pattern p es -> evalPat p es
                           
-- Shape and index related
      IAll -> return $ Idx_IAll 
      IIndex i -> do
        i' <- doEval i
        return $ (Idx_IIndex i') 
      IRange i j -> do
        i' <- doEval i
        j' <- doEval j
        return $ (Idx_IRange i' j') 
      ShapeZ -> return $ Shap []
      IndexZ -> return $ Idx []
      ShapeCons head tail -> do
        head' <- doEval head
        (Shap tail') <- doEval tail
        return $ Shap (head' : tail') 
      IndexCons head tail -> do
        head' <- doEval head
        (Idx tail') <- doEval tail
        return $ Idx (head' : tail') 



      --a -> error $ show a 
                                                
    appNotAFun = error "First argument of App is not a function"

    evalIota :: EvalResult -> E EvalResult
    evalIota sh =
      do --shape <- evalExtents e
         let  size  = sizeExtents sh
         return $ Array sh (V.generate size (\i -> (Scalar (VInt i))))

    -- Total size in number of elements 
    sizeExtents :: EvalResult -> Int
    sizeExtents (Shap xs) = foldl (\b (Scalar (VInt v))  -> v * b) 1 xs     
    
    evalOp :: Op -> [Exp] -> E EvalResult
    evalOp op1 [e1]    = undefined 
    evalOp op2 [e1,e2] =
      do e1' <- doEval e1
         e2' <- doEval e2
         case (e1',e2') of
           (Scalar (VInt i1), Scalar (VInt i2)) ->
             case op2 of
               Add -> return $ Scalar (VInt (i1 + i2))
               Sub -> return $ Scalar (VInt (i1 - i2))
               Mul -> return $ Scalar (VInt (i1 * i2))
               Div -> return $ Scalar (VInt (i1 `div` i2))
               Powi -> return $ Scalar (VInt (i1 ^ i2)) 

           (Scalar (VFloat f1), Scalar (VFloat f2)) ->
             case op2 of
               Add -> return $ Scalar (VFloat (f1 + f2))
               Sub -> return $ Scalar (VFloat (f1 - f2))
               Mul -> return $ Scalar (VFloat (f1 * f2))
               Div -> return $ Scalar (VFloat (f1 / f2))
               Powi -> error "Powi applied to float" 
           (a,b) -> error $ show a ++ " " ++ show op2 ++ " " ++ show b

-- -------------------------------------------------------
-- Evaluate Patterns 
-- -------------------------------------------------------
    evalPat p es =
      case (p,es) of
        (Generate,[exts,fun]) -> 
          do exts' <- doEval exts -- evalExtents exts
             (Function fun') <- doEval fun 
             let elems = sizeExtents exts'
             return $ Array exts' (V.generate elems
                              (\i -> fun' (fromScalarIdx exts'
                                           (Scalar (VInt i)))))
           

        (Map,[fun,e]) ->
          do e' <- doEval e
             fun' <- doEval fun 
             case e' of
               Array sh v ->
                 case fun' of
                   Function f -> return $ Array sh (V.map f v)
                   _ -> error "Argument to Map is not a function"
               _ -> error "Argument to Map is not an Array"
             -- The errors here are of the kind that type
             -- checking will catch 


      -- ZipWith needs to do some extents checking.
      -- TODO: check extents
      -- TODO: decide what combinations of shapes are allowed in a zipwith
      -- Currently requires that both inputs have exact same shape and extents 
        (ZipWith, [fun,e1,e2]) ->
          do e1' <- doEval e1
             e2' <- doEval e2
             fun' <- doEval fun
             case (e1',e2') of
               (Array sh1 v1, Array sh2 v2) ->
                 case fun' of
                   Function f ->
                     return $ Array sh1
                     (V.zipWith
                      (\x y -> let Function f' = f x in f' y) v1 v2) 
                   _ -> error "Argument to ZipWith is not a function"
               _ -> error "Argument to ZipWith is not an Array" 
                         


      -- what reduction to perform here ???
      -- Reduce should be augmented with a description of
      -- what dimension to reduce.
      -- Currently reduce over outermost dimension (reduce_rows)

      -- Reduction could actually be down to a scalar in ALL cases !
      -- It is then up to the programmer to "block" up the data into
      -- chunks before (map reduce) on the blocked structure, if some
      -- other reduction is desired. 

      -- Reduce now produces scalar 
        (Reduce, [fun,e_id,e]) ->
          do e'    <- doEval e
             e_id' <- doEval e_id 
             Function f <- doEval fun
             case e' of
               (Array (Shap sh) v) ->
                 case sh of
                   [] ->  return $ v V.! 0 --    Array sh v
                   -- any other shape, reduce array to a single value 
                   _ -> return $ V.foldr (\x y ->
                                           let Function f' = f x
                                           in f' y) e_id' v
                
               _ -> error "Argument to reduce is not an Array"


           

   
    doPrj (Array sh v) (Idx idx) = error "doPrj: not yet implemented"

    newShape :: [EvalResult] -> [EvalResult] -> [EvalResult]
    newShape [] [] = []
    newShape (s:sh) (i:idx) =
      case i of
        Idx_IAll -> s : newShape sh idx
        Idx_IIndex _ -> newShape sh idx 
        Idx_IRange (Scalar (VInt x))
                   (Scalar (VInt y))
          -> (Scalar $ VInt (y - x)) : newShape sh idx

    -- TODO: Turn a flattened index in the newShape
    --       into a flattened index in the old shape
    --  If this is only used to create the new V.Vector, it could return an int...     
    flatIndexShapeConvert :: [EvalResult] -> [EvalResult] -> EvalResult -> EvalResult
    flatIndexShapeConvert oldShape mapping idx = undefined 
      where
        doConv = undefined 
    

-- -------------------------------------------------------
-- toIdx, fromIdx 
-- -------------------------------------------------------

-- Fastest growing is outermost!
-- Thus all the reverses...
        

--       Shape         Shaped Index   Scalar 
toScalarIdx :: EvalResult -> EvalResult -> EvalResult
toScalarIdx (Shap sh) (Idx ix) = toIdx' (reverse sh) (reverse ix)
  where
    toIdx' [] [] = Scalar (VInt 0)
    toIdx' (Scalar (VInt s):ss) (Scalar (VInt i):is) =
      let (Scalar (VInt r)) = toIdx' ss is
      in  Scalar (VInt (r * s + i))
    
toScalarIdx _ _ = error "toIdx: error!" 

--         Shape             Scalar     (Shaped Index) 
fromScalarIdx :: EvalResult -> EvalResult -> EvalResult
fromScalarIdx (Shap sh) ix = Idx (reverse $ fromIdx' (reverse sh) ix)
  where
    --       Z  _
    -- lots of potential unmatched cases here ! 
    fromIdx' [] _  = []
    fromIdx' [Scalar (VInt x)] (Scalar (VInt i)) = [Scalar (VInt i)]
    fromIdx' (Scalar (VInt x):xs) (Scalar (VInt i)) =  (Scalar (VInt (i `rem` x)) :
                                          (fromIdx' xs (Scalar (VInt (i `quot` x)))))
fromIdx _ _ = error "fromIdx: error!"





-- -------------------------------------------------------
-- Validation of tuneparams 
-- -------------------------------------------------------

-- TODO: This is currently extremely slow.
--       Figure out why and fix it. 

checkTPSafety :: Exp -> IO ()
checkTPSafety e = quickCheck (tpSafe e) 

tpSafe :: Exp -> Property
tpSafe e = forAll envG
                  (\env1 -> forAll envG
                            (\env2 -> (eval env1 e' == eval env2 e' ))) 
  where
    (e',tps)  = extractTuneParams e 
    envG      = envGenerator tps  

envGenerator :: (M.Map Identifier TP) -> Gen Env
envGenerator m = do m' <- mapM (\(i,tp) -> do {tp' <- genTP tp; return (i,tp')}) (M.toList m)
                    return $ M.fromList m' 
  where
    genTP :: TP -> Gen EvalResult
    genTP (TPIntRange i j) = do v <- choose (i,j) 
                                return (Scalar (VInt v))   
    genTP TPBool           = do b <- arbitrary
                                return (Scalar (VBool b))
    genTP NumCores         = do v <- choose (1,2048)
                                return (Scalar (VInt v))
    genTP SIMDWidth        = do v <- choose (1, 8)
                                return (Scalar (VInt (v * 4)))
    genTP CacheSize        = do v <- choose (1, 256)
                                return (Scalar (VInt (16384 * 1024 * v)))
    genTP CacheLineSize    = do v <- choose (1, 32)
                                return (Scalar (VInt (1024 * v)))
    
                                        
                               

                                
                                
