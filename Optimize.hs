module Optimize where

class (TransferFunction a) => Optimizer a where
    -- Returns the optimized program as a new Cfg
    optimize :: Cfg a -> Cfg a
