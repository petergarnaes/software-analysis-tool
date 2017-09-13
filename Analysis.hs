module Analysis where
import Lattice
import BasicTypes
import Environment
import WhileLangAst
import TransferFunction
import Data.List
import Data.Map as Map (Map,empty,unionWith)

--analyse :: (TransferFunction a) => [ProgramPoint] -> Environment a ()
--analyse pps = evaluateProgramPoints pps (return ())

--runAnalysisOnce :: (TransferFunction a) => [ProgramPoint] -> (Map Ident a) -> (Map Ident a)
--runAnalysisOnce pps m = let (_,map) = runEnvironment (analyse pps) m in map

-- Tail recursively run until no changes
--runUntilNoChanges :: (TransferFunction a) => [ProgramPoint] -> (Map Ident a) -> (Map Ident a)
--runUntilNoChanges pps m = let 
--    m2 = runAnalysisOnce pps m 
--    in if m2 == m 
--    then m 
--    else runUntilNoChanges pps m2

--runCompleteAnalysis :: (TransferFunction a) => [ProgramPoint] -> a -> (Map Ident a)
--runCompleteAnalysis pps _ = runUntilNoChanges pps Data.Map.empty

invertCfg :: (TransferFunction a) => Cfg a -> Cfg a -> Cfg a
invertCfg (CfgPP m pp cfg) prev = invertCfg cfg (CfgPP m pp prev)
invertCfg (CfgIf m cIf cElse cfg) prev = invertCfg cfg (CfgIf m cIf cElse prev)
invertCfg (CfgWhile m cWh cfg) prev = invertCfg cfg (CfgWhile m cWh prev)
invertCfg (CfgExit m) prev = prev

invert :: (TransferFunction a) => Cfg a -> Cfg a
invert cfg = invertCfg cfg (CfgExit Map.empty)

analyseCfg :: (TransferFunction a) => Cfg a -> LatticeMap a -> (LatticeMap a,Cfg a)
analyseCfg (CfgPP _ pp cfg) map = let 
    (_,newMap) = runEnvironment (transfer pp (return ())) map
    (m,c) = analyseCfg cfg newMap
    in (m,CfgPP newMap pp c)
analyseCfg (CfgIf _ cfgIf cfgElse cfg) map = let
    (m1,c1) = analyseCfg cfgIf map
    (m2,c2) = analyseCfg cfgElse map
    newMap = unionWith bound m1 m2
    (m,c) = analyseCfg cfg newMap
    in (m,CfgIf newMap c1 c2 c)
analyseCfg (CfgWhile _ cfgWhile cfg) map = let
    (m1,c1) = analyseCfg cfgWhile map
    newMap = unionWith bound m1 map
    (m,c) = analyseCfg cfg newMap
    in (m,CfgWhile newMap c1 c)
analyseCfg (CfgExit _) map = (map,CfgExit map)

-- Dont think this is relevant with environment passed down
analyseTillNoChanges :: (TransferFunction a) => Cfg a -> LatticeMap a -> Cfg a
analyseTillNoChanges cfg map = let
    (m,c) = analyseCfg cfg map
    in if map == m
    then c
    else analyseTillNoChanges c m

--fullAnalysis :: (TransferFunction a) => Cfg a -> Cfg a
--fullAnalysis cfg = analyseTillNoChanges cfg Map.empty

fullAnalysis :: (TransferFunction a) => Cfg a -> Cfg a
fullAnalysis cfg = let (_,c) = analyseCfg cfg Map.empty in c
