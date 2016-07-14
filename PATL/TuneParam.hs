
module PATL.TuneParam where


-- Tuning parameters

data TP = TPIntRange Int Int 
        | TPBool
        | NumCores
        | SIMDWidth 
        | CacheSize
        | CacheLineSize 
          deriving (Eq, Show)
