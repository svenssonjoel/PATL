
module PATL.Value where


data Value = VFloat Float
           | VInt   Int
           | VBool Bool 
             deriving (Eq,Show)
