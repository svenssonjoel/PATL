
--
-- 
--

module PATL.AST where

--import PATL.Shape
import PATL.Value 
import PATL.TuneParam
import PATL.Operators

import qualified Data.Map as M
import Control.Monad.State 

type Size = Exp 

{- The Language -} 

type Identifier = String

data Def = Def Identifier Exp 

type Program = [Def] -- Top level program 

data Exp = -- Annotated with "top-level" types
           
          
           --  :: Int , Float  for example 
           Constant Value
           -- :: Int, Float, Array sh a, (a -> b) ... 
         | Var Identifier

           -- Shape and Index expressions
           -- :: Shape Int
         | Sh Exp
         | Ix Exp   
         | Z
         | Cons Exp Exp
         | IAll | IIndex Exp | IRange Exp Exp 

           -- :: Tune Int, Tune Bool 
         | TuneParam TP  -- tuning parameters

           -- Tuples :: (Int,Float) , (Array Int, Bool) ... 
         | Tuple [Exp]

           -- :: a -> b, a -> b -> c ... 
         | Op Op [Exp]

           -- Functions
           -- Lam :: a -> b 
         | Lam Identifier Exp
           
           -- App :: (a -> b) -> a -> b 
         | App Exp Exp

           -- Let bindings
           -- Let :: b 
         | Let Identifier Exp Exp


           -- Create an array (todo more ways)
           -- Iota :: (Shape Int) -> Array sh Int 
         | Iota Exp  -- what does this mean for multidim arrays ?


           -- Project from N-dimensional arrays
           -- Prj :: Array sh a -> Index -> Array sh' a
           -- Prj :: Array sh a -> Index -> a
           -- Maybe also Prj :: sh -> Index -> Exp 
         | Prj Exp Exp 

           -- Size of Exp (array or scalar)
           -- SizeOf :: Array sh a -> sh
           -- SizeOf :: Int, Float -> Int
           -- ... 
         | SizeOf Exp

           -------------------------------------------------------
           -- Patterns
           ------------------------------------------------------- 
           
           -- Generate :: (Shape Int) -> (Index -> a) -> Array sh a 
         | Generate Exp Exp
           
           -- Map :: (a -> b) -> Array sh a -> Array sh b 
         | Map Exp Exp
           
           -- ZipWith :: (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
         | ZipWith Exp Exp Exp
           
           -- Reduction is a tricky one!
           -- Reduce :: (a -> b -> b) -> b -> Array (sh:.i) a -> Array sh b
           -- Reduce :: (a -> b -> b) -> b -> Array (sh:.x:.rest) a -> Array (sh:.rest) b ??? 
           -- Reduce :: (a -> b -> b) -> b -> Array sh a -> b
           -- How to specify what reduction to use ??

           -- I think now that there will be only one Reduce!
           -- Reduce :: (a -> b -> b) -> b -> Array sh a -> b
         | Reduce Exp Exp Exp        
           -- | Permute ?
           -- | Scatter
           -- | Gather
           -- | Scan 
           
           deriving (Eq, Show)


foldExp :: ( a -> Exp -> a) -> a -> Exp -> a
foldExp f a e = doIt a e
  where 
    doIt a e@(IIndex e1) = f (doIt a e1) e 
    doIt a e@(IRange e1 e2) = f (foldl doIt a [e1,e2]) e 
    doIt a e@(Tuple es) = f (foldl doIt a es) e
    doIt a e@(Op _ es)  = f (foldl doIt a es) e
    doIt a e@(Lam _ e1)  = f (doIt a e1) e
    doIt a e@(App e1 e2) = f (foldl doIt a [e1,e2]) e
    doIt a e@(Let _ e1 e2) = f (foldl doIt a [e1,e2]) e
    doIt a e@(Iota e1) = f (doIt a e1) e
    doIt a e@(Prj e1 e2) = f (foldl doIt a [e1,e2]) e
    doIt a e@(SizeOf e1) = f (doIt a e1) e
    doIt a e@(Generate e1 e2) = f (foldl doIt a [e1,e2]) e
    doIt a e@(Map e1 e2) = f (foldl doIt a [e1,e2]) e
    doIt a e@(ZipWith e1 e2 e3) = f (foldl doIt a [e1,e2,e3]) e
    doIt a e@(Reduce e1 e2 e3) = f (foldl doIt a [e1,e2,e3]) e 
    doIt a e = f a e



-- -------------------------------------------------------
-- Extraction of tuning parameters
-- -------------------------------------------------------

type MS a = State (M.Map Identifier TP, Int) a

addTP :: TP -> MS Identifier   
addTP tp = do
  (m,i) <- get
  let ident = "tp" ++ show i
      m' = M.insert ident tp m
  put (m',i+1)
  return ident 
  
-- extract tuning parameters
-- replace in AST with a variable
-- This may be something like "traverse" 
extractTuneParams :: Exp -> (Exp, M.Map Identifier TP)  

extractTuneParams e = let (a,(m,i)) = runState (doIt e) (M.empty,0)
                      in  (a,m) 
  where
    doIt :: Exp -> MS Exp 
    doIt (TuneParam tp) =
      do ident <- addTP tp
         return (Var ident)
    doIt (IIndex e1) =  IIndex <$> (doIt e1)
    doIt (IRange e1 e2) = IRange <$> doIt e1 <*> doIt e2
    doIt (Tuple es) = Tuple <$> mapM doIt es
    doIt (Op op es) = Op op <$> mapM doIt es
    doIt (Lam ident e1) = Lam ident <$> doIt e1
    doIt (App e1 e2) = App <$> doIt e1 <*> doIt e2
    doIt (Let ident e1 e2) = Let ident <$> doIt e1 <*> doIt e2
    doIt (Iota e1) = Iota <$> doIt e1
    doIt (Prj e1 e2) = Prj <$> doIt e1 <*> doIt e2
    doIt (SizeOf e1) = SizeOf <$> doIt e1
    doIt (Generate e1 e2) = Generate <$> doIt e1 <*> doIt e2
    doIt (Map e1 e2) = Map <$> doIt e1 <*> doIt e2
    doIt (ZipWith e1 e2 e3) = ZipWith <$> doIt e1 <*> doIt e2 <*> doIt e3
    doIt (Reduce e1 e2 e3) = Reduce <$> doIt e1 <*> doIt e2 <*> doIt e3 
     
-- non recursive cases 
    doIt  e = return e  




  
------------------------------------------------------------
-- Examples
-- myArray :: Exp
-- myArray = Iota (Cons (Constant (VInt 100)) Z)

-- myFun :: Exp
-- myFun = Lam "a" (Op Add [(Var "a"),(Constant (VInt 1))])

-- myPrg :: Exp
-- myPrg = Map myFun myArray
                 

------------------------------------------------------------
-- Blocked Matrix Mult sketch (for square matrices)
-- Cheating here and there 

-- blocked_mmult =
--   Lam "m1" $
--   Lam "m2" $ 
                      
--   Let "tp" (TuneParam TPInt) $
--   Let "blocked_m1" (Block (Square (Var "tp")) (Var "m1")) $
--   Let "blocked_m2" (Block (Square (Var "tp")) (Var "m2")) $
--   Let "block_rows_m1" (ZipWith extract_row (Var "blocked_m1") (Iota (Sh (Z:.(Var "bs"))))) $
--   Let "block_cols_m2" (ZipWith extract_col (Var "blocked_m2") (Iota (Sh (Z:.(Var "bs"))))) $
--   Let "bs" (SizeOf (Var "blocked_m1")) $ -- assuming square
--   Let "outer_gen" (Lam "m1"    
--                    $ Lam "m2" 
--                    $ Lam "row"
--                    $ Lam "col"
--                    $ Reduce (Var "add") (Constant (VInt 0)) (ZipWith (Var "mmult")
--                                          (apply extract_row [Var "row", Var "m1"])
--                                          (apply extract_col [Var "col", Var "m2"]))) $
--   Let "gen_func" (Lam "m1"
--                   $ Lam "m2"
--                   $ Lam "i"
--                   $ Lam "j"
--                   $ Reduce (Var "add") (Constant (VInt 0)) 
--                      (ZipWith (Var "multiply")
--                       (apply extract_row [Var "i", Var "m1"])
--                       (apply extract_col [Var "j", Var "m2"]))) $
--   Let "mmult" (Lam "m1"
--                $ Lam "m2"
--                $ Generate (Sh (Z:.(Var "tp"):.(Var "tp")))
--                  $ apply (Var "gen_fun") [Var "m1", Var "m2"]) $
--   Let "multiply" (Lam "x"
--                   $ Lam "y"
--                   $ Op Mul [Var "x",Var "y"]) $
--   Let "add" (Lam "x"
--              $ Lam "y"
--              $ Op Add [Var "x", Var "y"]) $

--   UnBlock $ Generate (Sh (Z:.(Var "bs"):.(Var "bs")))
--           $ apply (Var "outer_gen") [Var "blocked_m1",
--                                     Var "blocked_m2"]
               
-- extract_row :: Exp
-- extract_row = Lam "arr"
--               $ Lam "y"
--               $ Prj (Var "arr") (Z:.IAll:.(IIndex (Var "y")))

-- extract_col :: Exp
-- extract_col = Lam "arr"
--               $ Lam "y"
--               $ Prj (Var "arr") (Z:.(IIndex (Var "y")):.IAll)




-- apply :: Exp -> [Exp] -> Exp
-- apply e [] = e
-- apply e (x:xs) = apply (App e x) xs

-- mmult_example =
--   let c = Constant (VInt 128) 
--   in App (App blocked_mmult (Iota (Sh (Z:.c:.c)))) (Iota (Sh (Z:.c:.c)))


-- ------------------------------------------------------------
-- -- Reduction can be expressed in many ways.

-- myReduce = Lam "arr"
--            $ Let "add" (Lam "x"
--                         $ Lam "y"
--                         $ Op Add [Var "x", Var "y"])
--            $ Reduce (Var "add") (Constant (VInt 0)) (Var "arr")


-- -- 2 level reduce that can make better use of cache or local memory 
-- myReduce2 = Lam "arr"
--             $ Let "add" (Lam "x"
--                          $ Lam "y"
--                          $ Op Add [Var "x", Var "y"])
--             $ Let "chunk_size" (TuneParam TPInt) 
--             $ Reduce (Var "add") (Constant (VInt 0))
--                 (Map (Lam "chunk" (Reduce (Var "add") (Constant (VInt 0))(Var "chunk")))
--                   (Block (Chunk (Var "chunk_size"))  (Var "arr")))


------------------------------------------------------------
-- Fusion and Compilation sketches






