{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}


module Processor.Tool.InteractiveDebugger (
        -- * Interactive Debugger driver
          runIdbIO
        -- * Interactive Debugger usage
        -- $idbnote
  ) where

import System.IO
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.List (intercalate, elemIndex)
import Data.Char (toUpper)
import Data.List.Split (splitOn)
import Text.Printf (printf)

import Processor.Core.Instruction
import Processor.Core.Memory
import Processor.Core.State
import Processor.Tool.Debugger


----------------------------------------
--  driver
----------------------------------------
-- | interactive debugger driver.
--
-- Example:
--
-- >  > runIdbIO [TrcInst] []  [(0,insts)] []
-- >  For help, type "help".
-- >  (idb) info reg
-- >  pc : 0
-- >  gr : [0,0,0,0,0,0,0,0]
-- >  fl : [False,False]
-- >  
-- >  (idb) s
-- >  TrcInst:        pc : 0  MOVI R0 0
-- >  
-- >  (idb) s
-- >  TrcInst:        pc : 1  MOVI R1 1
-- >  
-- >  (idb) b 4
-- >  Num  Enb What
-- >  1    y   pc == 4
-- >  
-- >  (idb) c
-- >  TrcInst:        pc : 2  MOVI R2 2
-- >  
-- >  TrcInst:        pc : 3  MOVI R3 3
-- >  
-- >  (idb) x/10 0
-- >  0x00000000: 0x00000000 0x00000000 0x00000000 0x00000000
-- >  0x00000004: 0x00000000 0x00000000 0x00000000 0x00000000
-- >  0x00000008: 0x00000000 0x00000000
-- >  
-- >  (idb) q
--
-- see "help" command
--
runIdbIO :: [DbgTrc] -> [DbgBrk] -> InstImage -> DataImage -> IO ()
runIdbIO dbgtrc dbgbrk insts vals = 
    do putStrLn "For help, type \"help\"."
       loop initStat "" (setBrkTable dbgbrk)

    where
      initStat = initCpuStateMem insts vals
      loop s prev t = do
          putStr "(idb) "
          hFlush stdout
          a <- getLine
          let a' = (if a == "" then prev else a)
          let dbgbrk' = getEnableBrkTable t
          case (parseCommand a') of
            [""]    -> loop s a' t
            ["q"]   -> return ()
            ["run"] -> cmdRun initStat a' t dbgtrc dbgbrk'
            ["c"]   -> cmdRun s        a' t dbgtrc dbgbrk'
            ["s"]   -> cmdRun s        a' t dbgtrc (BrkOne : dbgbrk') 

            ("info":"reg":_) -> printRegs s        >> loop s a' t
            ["disas"]        -> printDisasmCr s    >> loop s a' t
            ["disas", ad]    -> printDisasmAd ad s >> loop s a' t

            ["x", ad]               -> printDmem "1" ad s >> loop s a' t
            [('x':'/':cnt), ad]     -> printDmem cnt ad s >> loop s a' t
            ["p", ('*':ad)]         -> printDmem "1" ad s >> loop s a' t
            ["p", ('*':ad), "=", v] -> loop (setDmem ad v s) a' t

            ["info", "b"]  -> printBrkTable t >> loop s a' t
            ["delete", n]  -> cmdBrkUtil s a' deleteBrkTable  n
            ["enable", n]  -> cmdBrkUtil s a' enableBrkTable  n
            ["disable", n] -> cmdBrkUtil s a' disableBrkTable n

            ["b", n]            -> cmdBrkWch s a' t (BrkPc BEQ ((read n)::Int))
            ["watch", d, o, v]  -> cmdBrkWch s a' t (parseWatchCmd d o v)

            ["help"]   -> printUsage >> loop s a' t
            xs         -> printUnknown xs >> printUsage >> loop s a' t

          where
            cmdRun st cmd tbl trc brk =
                let (l',st') = runState (evalProgDbg trc brk) st
                in  B.putStr l' >> loop st' cmd tbl
            cmdBrkUtil st cmd f n =
                let n' = (read n) - 1
                    t' = f n' t
                in  printBrkTable t' >> loop st cmd t'
            cmdBrkWch st cmd tbl b =
                let tbl' = addBrkTable (True, b) tbl
                in  printBrkTable tbl' >> loop st cmd tbl'


----------------------------------------
--  sub commands
----------------------------------------
-- parse utility
parseCommand :: String -> [String]
parseCommand = splitOn " "


-- usage
printUsage :: IO ()
printUsage = putStr "List of commands:\n\
                    \\n\
                    \q\t-- Exit debugger\n\
                    \help\t-- Print list of commands\n\
                    \run\t-- Start debugged program\n\
                    \s\t-- Step program\n\
                    \c\t-- Continue program being debugged\n\
                    \x\t-- Examin memory: x(/COUNT) ADDRESS\n\
                    \info reg\t-- List of registers\n\
                    \disas\t-- Disassemble: disassemble (ADDRESS)\n\
                    \info b\t-- Status of breakpoints\n\
                    \disable\t-- Disable breakpoint: disable NUMBER\n\
                    \enable\t-- Enable breakpoint: enable NUMBER\n\
                    \delete\t-- Delete breakpoint: delete NUMBER\n\
                    \b\t-- Set breakpoint: b ADDRESS\n\
                    \watch\t-- Set a watchpoint. example:\n\
                    \     \t     data memory -- watch *0x80 != 10\n\
                    \     \t     pc          -- watch pc > 3\n\
                    \     \t     register    -- watch r7 == 3\n\
                    \p\t-- Print memory value: p *ADDRESS\n\
                    \p\t-- Set memory value: p *ADDRESS = VALUE\n\
                    \\n"

printUnknown :: [String] -> IO ()
printUnknown xs = putStr $ "unknown command : " ++ (show $ unwords xs) ++ "\n\n"


-- info register utility
printRegs :: CpuState -> IO ()
printRegs s = putStr $ concat [ "pc : ",   (show . pcFromCpuState $ s)
                              , "\ngr : ", (show . grFromCpuState $ s)
                              , "\nfl : ", (show . flFromCpuState $ s), "\n\n" ]

-- disassemble utility
printDisasmCr :: CpuState -> IO ()
printDisasmCr s = printDisasm (pcFromCpuState s) 16 s

printDisasmAd :: String -> CpuState -> IO ()
printDisasmAd ad s = printDisasm (read ad) 16 s

printDisasm :: Int -> Int -> CpuState -> IO ()
printDisasm ad cnt s = putStr $
                         pprInst ad $ extructImems (imemFromCpuState s) ad cnt

pprInst :: Int -> [Inst] -> String
pprInst _  [] = []
pprInst ad xs = concat [ ppr0x08x ad, ": " , show y, "\n"
                       , pprInst (ad + 1) zs]
                  where (y:zs) = xs


-- memory access utility
-- TODO address range check
printDmem :: String -> String -> CpuState -> IO ()
printDmem cnt ad s = putStr $ pprDmem 4 ad' v
    where ad' = read ad
          v = extructDmems (dmemFromCpuState s) ad' (read cnt)

pprDmem :: Int -> Int -> [Int] -> String
pprDmem _ _  [] = []
pprDmem c ad xs = ppr0x08x ad ++ ": " ++
                  (unwords $ map ppr0x08x ys) ++ "\n" ++
                  pprDmem c (ad + c) zs
                  where (ys,zs) = splitAt c xs

ppr0x08x :: Int -> String
ppr0x08x = printf "0x%08x"

-- p set utility
setDmem :: String -> String -> CpuState -> CpuState
setDmem ad val = execState (updateDmem (read ad) (read val))



----------------------------------------
--  breakpoint and watchpoint
----------------------------------------
-- TODO: refuctoring!

type BrkTable = [(Bool, DbgBrk)]

-- set, get, print BrkTable
setBrkTable :: [DbgBrk] -> BrkTable
setBrkTable = zip (repeat True)

getEnableBrkTable :: BrkTable -> [DbgBrk]
getEnableBrkTable = map snd . filter ((== True) . fst)

printBrkTable :: BrkTable -> IO ()
printBrkTable xs = putStrLn $ "Num  Enb What\n" ++ pprBrkTable xs ++ "\n"

pprBrkTable :: BrkTable -> String
pprBrkTable xs = intercalate "\n" $ zipWith f ([1..]::[Int]) xs
  where f n (b, ibrk) = (show n) ++ "    " ++ (pprEnb b) ++ " " ++ (pprBrk ibrk)
        pprEnb True  = "y  "
        pprEnb False = "n  "
        pprBrk = showDbgBrk

showDbgBrk :: DbgBrk -> String
showDbgBrk (BrkNon) = "non breakpoint"
showDbgBrk (BrkOne) = "allways breakpoint"
showDbgBrk (BrkPc o ad) = concat [ "pc ", showDbgOrd o, " ", show ad ]
showDbgBrk (BrkDmem mem o ad) = concat [ "*", show mem, " "
                                       , showDbgOrd o, " ", show ad ]
showDbgBrk (BrkGReg reg o ad) = concat [ show reg, " "
                                       , showDbgOrd o, " ", show ad ]

showDbgOrd :: DbgOrd -> String
showDbgOrd BEQ = "=="
showDbgOrd BNE = "!="
showDbgOrd BLT = "<"
showDbgOrd BLE = "<="
showDbgOrd BGT = ">"
showDbgOrd BGE = ">="


-- add, delete, enable, disable element with BrkTable
addBrkTable :: (Bool, DbgBrk) -> BrkTable -> BrkTable
addBrkTable x xs = xs ++ [x]

deleteBrkTable :: Int -> BrkTable -> BrkTable
deleteBrkTable _ []     = []
deleteBrkTable 0 (_:xs) = xs
deleteBrkTable n (x:xs) = x : deleteBrkTable (n-1) xs

enableBrkTable, disableBrkTable :: Int -> BrkTable -> BrkTable
enableBrkTable  = setFstN True
disableBrkTable = setFstN False

setFstN :: a -> Int -> [(a,b)] -> [(a,b)]
setFstN _ _ [] = []
setFstN a 0 ((_,b):xs) = (a,b) : xs
setFstN a n (x:xs)     = x : (setFstN a (n-1) xs)


-- watch command parse utility
-- TODO: string check
parseWatchCmd :: String -> String -> String -> DbgBrk
parseWatchCmd ('*':xs) o v = BrkDmem (read xs) (strToDbgOrd o) (read v)
parseWatchCmd "pc"     o v = BrkPc   (strToDbgOrd o) (read v)
parseWatchCmd reg      o v = BrkGReg (strToGReg reg) (strToDbgOrd o) (read v)

-- from Assembler.hs  TODO: refactoring!
strToGReg :: String -> GReg
strToGReg  x = case (elemIndex x gregNames) of
                 Just n  -> toEnum n
                 Nothing -> error $ "strToGReg" ++ (show x)

gregNames :: [String]
gregNames  = map ((map toUpper) . show)
               [(minBound :: GReg) .. (maxBound :: GReg)]

strToDbgOrd :: String -> DbgOrd
strToDbgOrd "==" = BEQ
strToDbgOrd "!=" = BNE
strToDbgOrd "<"  = BLT
strToDbgOrd "<=" = BLE
strToDbgOrd ">"  = BGT
strToDbgOrd ">=" = BGE
strToDbgOrd _    = BEQ   -- TODO: check




-- $idbnote
-- 
-- Usage:
-- 
-- >  q         -- Exit debugger
-- >  help      -- Print list of commands
-- >  run       -- Start debugged program
-- >  s         -- Step program
-- >  c         -- Continue program being debugged
-- >  x         -- Examin memory: x(/COUNT) ADDRESS
-- >  info reg  -- List of registers
-- >  disas     -- Disassemble: disassemble (ADDRESS)
-- >  info b    -- Status of breakpoints
-- >  disable   -- Disable breakpoint: disable NUMBER
-- >  enable    -- Enable breakpoint: enable NUMBER
-- >  delete    -- Delete breakpoint: delete NUMBER
-- >  b         -- Set breakpoint: b ADDRESS
-- >  watch     -- Set a watchpoint. example:
-- >                 data memory -- watch *0x80 != 10
-- >                 pc          -- watch pc > 3
-- >                 register    -- watch r7 == 3
-- >  p         -- Print memory value: p *ADDRESS
-- >  p         -- Set memory value: p *ADDRESS = VALUE
-- >  


