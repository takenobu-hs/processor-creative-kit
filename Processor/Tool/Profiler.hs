
{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}


module Processor.Tool.Profiler (
        -- * Profiler driver
          runProf
        , runProfIO
        , prof
        -- * Data type
        , ProfMode(..)
  ) where

import qualified Data.ByteString.Char8 as B
import Data.List (foldl', sortBy, nub)
import qualified Data.Map as Map
import Data.Function (on)
import Text.Printf (printf)

import Processor.Core.Memory
import Processor.Tool.Debugger


----------------------------------------
--  driver
----------------------------------------
-- | profile mode for 'prof', 'runProf' and 'runProfIO'
data ProfMode = ProfInst    -- ^ instruction profile
              | ProfPC      -- ^ pc profile
              | ProfCall    -- ^ call profile
              | ProfBranch  -- ^ branch, jump, call profile
              | ProfLoad    -- ^ memory load profile
              | ProfStore   -- ^ memory store profile
--            | ProfDmem
--            | ProfStat
              deriving Eq


-- | run profiler
--
-- Example: instruction count profile
--
-- >  > runProf [ProfInst] [(0,[MOVI R1 0, MOVI R2 8, ST R1 R2, HALT])] []
-- >   instruction profile:
-- >   
-- >     MOVI  2
-- >     HALT  1
-- >     ST    1
-- >   
-- >     total 4
--
-- Example: memory store profile
--
-- >  > runProf [ProfStore] [(0,insts)] []
-- >  Memory store address profile:
-- >  
-- >    address       count
-- >    0x00000000    1
-- >    0x00000001    1
-- >    0x00000002    1
-- >    0x00000003    1
-- >    0x00000004    1
-- >    0x00000005    1
-- >    0x00000006    1
-- >  
-- >    total         7
--   
-- Example: branch,jump,call profile
--
-- >  > runProf [ProfBranch] [(0,insts)] []
-- >  Branch/Jump/Call target profile:
-- >  
-- >    address       count
-- >    0x00000007    6
-- >  
-- >    total 6
-- >  
-- >  
-- >  Branch/Jump/Call direction profile:
-- >  
-- >    T/N   count
-- >    Taken 6
-- >    Not   1
-- >  
-- >    total 7
--   
runProf :: [ProfMode] -> InstImage -> DataImage -> String
runProf profmd insts vals =
    let dbgtrc = modeProfToTrc profmd
        (trc, _) = runDbg dbgtrc [] insts vals
    in  prof profmd trc


-- | run profiler for IO stdout
--
-- Example:
--
-- > > runProfIO [ProfInst] [(0,[MOVI R1 0, MOVI R2 8, ST R1 R2, HALT])] []
-- >  instruction profile:
-- >  
-- >    MOVI  2
-- >    HALT  1
-- >    ST    1
-- >  
-- >    total 4
--
runProfIO :: [ProfMode] -> InstImage -> DataImage -> IO ()
runProfIO profmd insts vals = putStr $ runProf profmd insts vals


-- | profiler body.
--
-- Example:
--
-- > > prof [ProfInst] $ fst $ runDbg [TrcInst] [] [(0,insts)] []
-- >  instruction profile:
-- >  
-- >    MOVI  2
-- >    HALT  1
-- >    ST    1
-- >  
-- >    total 4
--
prof :: [ProfMode] -> B.ByteString -> String
prof profmd trclog = concatMap (`profOne` trclog) profmd

-- profFile :: [ProfMode] -> File.Path -> String


modeProfToTrc :: [ProfMode] -> [DbgTrc]
modeProfToTrc profmd = nub $ foldl' (\v x -> gen x ++ v) [] profmd
    where gen ProfInst   = [TrcInst]
          gen ProfCall   = [TrcCall]
          gen ProfBranch = [TrcBranch]
          gen ProfLoad   = [TrcLoad]
          gen ProfStore  = [TrcStore]
          gen _          = []

----------------------------------------
--  profiler
----------------------------------------
profOne :: ProfMode -> B.ByteString -> String
profOne ProfInst   trclog = pprInstCounts trclog
profOne ProfCall   trclog = pprCallCounts trclog
profOne ProfBranch trclog = pprBranchAdCounts trclog ++ pprBranchTNCounts trclog
profOne ProfLoad   trclog = pprLoadCounts trclog
profOne ProfStore  trclog = pprStoreCounts trclog
profOne _         _      = ""



----------------------------------------
--  each profile and pretty print
----------------------------------------
-- instruction count
pprInstCounts :: B.ByteString -> String
pprInstCounts trclog = pprTrcCounts hfmt cfmt tfmt counts 50
    where hfmt = "\ninstruction profile:\n\n"
          cfmt = "  %s\t%s\n"
          tfmt = "\n  total %s\n\n"
          counts = calcTrcCounts "TrcInst:" 2 0 trclog

-- memory load count
pprLoadCounts :: B.ByteString -> String
pprLoadCounts trclog = pprTrcCounts hfmt cfmt tfmt counts 50
    where hfmt = "\nMemory load address profile:\n\n  address\tcount\n"
          cfmt = "  %s\t%s\n"
          tfmt = "\n  total   \t%s\n\n"
          counts = fstHex $ calcTrcCounts "TrcLoad:" 1 2 trclog

-- memory load count
pprStoreCounts :: B.ByteString -> String
pprStoreCounts trclog = pprTrcCounts hfmt cfmt tfmt counts 50
    where hfmt = "\nMemory store address profile:\n\n  address\tcount\n"
          cfmt = "  %s\t%s\n"
          tfmt = "\n  total   \t%s\n\n"
          counts = fstHex $ calcTrcCounts "TrcStore:" 1 2 trclog

-- call count
pprCallCounts :: B.ByteString -> String
pprCallCounts trclog = pprTrcCounts hfmt cfmt tfmt counts 50
    where hfmt = "\nCall target profile:\n\n  address\tcount\n"
          cfmt = "  %s\t%s\n"
          tfmt = "\n  total   \t%s\n\n"
          counts = fstHex $ calcTrcCounts "TrcCall:" 1 2 trclog

-- branch address count
pprBranchAdCounts :: B.ByteString -> String
pprBranchAdCounts trclog = pprTrcCounts hfmt cfmt tfmt counts 50
    where hfmt = "\nBranch/Jump/Call target profile:\n\n  address\tcount\n"
          cfmt = "  %s\t%s\n"
          tfmt = "\n  total\t%s\n\n"
          counts = fstHex $ calcTrcCounts "TrcBranch:" 1 2 
                          $ filterLines 2 0 (== "Taken") trclog

-- branch direction count
pprBranchTNCounts :: B.ByteString -> String
pprBranchTNCounts trclog = pprTrcCounts hfmt cfmt tfmt counts 50
    where hfmt = "\nBranch/Jump/Call direction profile:\n\n  T/N\tcount\n"
          cfmt = "  %s\t%s\n"
          tfmt = "\n  total\t%s\n\n"
          counts = calcTrcCounts "TrcBranch:" 2 0 trclog



----------------------------------------
-- sttistics utility 
----------------------------------------
                      -- key          value
type KeyCounts = Map.Map B.ByteString Int

initKeyCounts :: KeyCounts
initKeyCounts = Map.empty

addKey :: KeyCounts -> B.ByteString -> KeyCounts
addKey kc key = Map.insertWithKey (\_ _ o -> o + 1) key 1 kc

calcKeyCounts :: [B.ByteString] -> KeyCounts
calcKeyCounts = foldl' addKey initKeyCounts



----------------------------------------
-- utility 
----------------------------------------
-- counter
pprTrcCounts :: String -> String -> String -> [(B.ByteString, Int)]
                  -> Int -> String
pprTrcCounts hfmt cfmt tfmt counts lim = hfmt ++ pprCounts ++ pprTotal
    where pprCounts = concatMap ppr (take lim counts)
          ppr (op, n) = printf cfmt (B.unpack op) (show n)
          total  = show . sum . map snd $ counts
          pprTotal = printf tfmt total

calcTrcCounts ::  B.ByteString -> Int -> Int -> B.ByteString
                    -> [(B.ByteString, Int)]
calcTrcCounts label ntab nspc =
                 sortBy (flip (compare `on` snd)) . Map.toList
               . calcKeyCounts
               . map (extructField ntab nspc)
               . extructLines label

extructLines :: B.ByteString -> B.ByteString -> [B.ByteString]
extructLines prefix = filter (prefix `B.isPrefixOf`)
                    . filter (`notElem` [""])
                    . B.lines

extructField :: Int -> Int -> B.ByteString -> B.ByteString
extructField ntab nspc xs = field
    where ftab  = B.splitWith (== '\t') xs
          fspc
            | null ftab = []
            | otherwise = B.splitWith (== ' ') (ftab !! ntab)
          field 
            | null fspc = ""
            | otherwise = fspc !! nspc

filterLines :: Int -> Int -> (B.ByteString -> Bool) ->
                 B.ByteString -> B.ByteString
filterLines ntab nspc f = B.unlines . filter cond . B.lines
    where cond xs = f $ extructField ntab nspc xs


-- converter
fstHex :: [(B.ByteString, Int)] -> [(B.ByteString, Int)]
fstHex = pairMap toHexBS id

toHexBS :: B.ByteString -> B.ByteString
toHexBS = B.pack . printf "0x%08x" . (read :: String -> Int) . B.unpack

pairMap :: (a->a1) -> (b->b1) -> [(a, b)] -> [(a1, b1)]
pairMap fa fb = map (\(a,b) -> (fa a, fb b))



