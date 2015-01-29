{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}


module Language.Pck.Tool.InteractiveDebugger (
        -- * Interactive Debugger driver
          runIdbIO
        -- * Interactive Debugger usage
        -- $idbnote
  ) where

import System.IO
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.List (intercalate, elemIndex, sortBy)
import Data.Char (toLower)
import Text.Printf (printf)

import Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8
import Control.Applicative ((<$>), (<*>), (<|>), (<$), (*>))


import Language.Pck.Cpu.Instruction
import Language.Pck.Cpu.Memory
import Language.Pck.Cpu.State
import Language.Pck.Tool.Debugger


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
-- please see "help" command
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
          let a' = (if a == "" then prev else map toLower a)
          let dbgbrk' = getEnableBrkTable t
          case (parseOnly line (B.pack a')) of
            Left _  -> printUnknown a' >> printUsage >> loop s a' t
            Right x ->
              case x of
                CBlank           -> loop s a' t
                CQuit            -> return ()
                CRun             -> exeRun initStat a' t dbgtrc dbgbrk'
                CC               -> exeRun s a' t dbgtrc dbgbrk'
                CS               -> exeRun s a' t dbgtrc (BrkOne : dbgbrk') 

                CInfoReg         -> printRegs s        >> loop s a' t
                CDisasCr         -> printDisasmCr s    >> loop s a' t
                CDisasAd ad      -> printDisasmAd ad s >> loop s a' t

                CX n ad          -> printDmem n ad s >> loop s a' t
                CPw ad v         -> loop (setDmem ad v s) a' t

                CInfoB           -> printBrkTable t >> loop s a' t
                CDelete  n       -> exeBrkUtil s a' deleteBrkTable  n
                CEnable  n       -> exeBrkUtil s a' enableBrkTable  n
                CDisable n       -> exeBrkUtil s a' disableBrkTable n

                CB n             -> exeBW s a' t (BrkPc BEQ n)
                CWatchPc     o v -> exeBW s a' t (BrkPc o v)
                CWatchGReg d o v -> exeBW s a' t (BrkGReg d o v)
                CWatchDmem d o v -> exeBW s a' t (BrkDmem d o v)

                CHelp -> printUsage >> loop s a' t

          where
            exeRun st cmd tbl trc brk =
                let (l',st') = runState (evalProgDbg trc brk) st
                in  B.putStr l' >> loop st' cmd tbl
            exeBrkUtil st cmd f n =
                let t' = f (n - 1) t
                in  printBrkTable t' >> loop st cmd t'
            exeBW st cmd tbl b =
                let tbl' = addBrkTable (True, b) tbl
                in  printBrkTable tbl' >> loop st cmd tbl'


----------------------------------------
--  sub commands
----------------------------------------
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

printUnknown :: String -> IO ()
printUnknown xs = putStr $ "unknown command : " ++ (show xs) ++ "\n\n"


-- info register utility
printRegs :: CpuState -> IO ()
printRegs s = putStr $ concat [ "pc : ",   (show . pcFromCpuState $ s)
                              , "\ngr : ", (show . grFromCpuState $ s)
                              , "\nfl : ", (show . flFromCpuState $ s), "\n\n" ]

-- disassemble utility
printDisasmCr :: CpuState -> IO ()
printDisasmCr s = printDisasm (pcFromCpuState s) 16 s

printDisasmAd :: Int -> CpuState -> IO ()
printDisasmAd ad s = printDisasm ad 16 s

printDisasm :: Int -> Int -> CpuState -> IO ()
printDisasm ad cnt s = putStr $
                         pprInst ad $ extractImems (imemFromCpuState s) ad cnt

pprInst :: Int -> [Inst] -> String
pprInst _  [] = []
pprInst ad xs = concat [ ppr0x08x ad, ": " , show y, "\n"
                       , pprInst (ad + 1) zs]
                  where (y:zs) = xs


-- memory access utility
printDmem :: Int -> Int -> CpuState -> IO ()
printDmem cnt ad s = putStr $ pprDmem 4 ad $
                       extractDmems (dmemFromCpuState s) ad cnt

pprDmem :: Int -> Int -> [Int] -> String
pprDmem _ _  [] = []
pprDmem c ad xs = ppr0x08x ad ++ ": " ++
                  (unwords $ map ppr0x08x ys) ++ "\n" ++
                  pprDmem c (ad + c) zs
                  where (ys,zs) = splitAt c xs

ppr0x08x :: Int -> String
ppr0x08x = printf "0x%08x"

-- p set utility
setDmem :: Int -> Int -> CpuState -> CpuState
setDmem ad val = execState (updateDmem ad val)



----------------------------------------
--  breakpoint and watchpoint
----------------------------------------
type BrkTable = [(Bool, DbgBrk)]

-- set, get, print the BrkTable
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
showDbgBrk (BrkPc o ad) = printf "PC %s %d  (PC %s 0x%x)" o' ad o' ad
                            where o' = showDbgOrd o
showDbgBrk (BrkDmem mem o ad) = printf "*%d %s %d  (*0x%x %s 0x%x)"
                                  mem o' ad mem o' ad
                                    where o' = showDbgOrd o
showDbgBrk (BrkGReg reg o ad) = printf "%s %s %d  (%s %s 0x%x)"
                                  reg' o' ad reg' o' ad
                                    where reg' = show reg
                                          o' = showDbgOrd o

showDbgOrd :: DbgOrd -> String
showDbgOrd BEQ = "=="
showDbgOrd BNE = "!="
showDbgOrd BLT = "<"
showDbgOrd BLE = "<="
showDbgOrd BGT = ">"
showDbgOrd BGE = ">="


-- add, delete, enable, disable element with the BrkTable
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



----------------------------------------
-- command parser
----------------------------------------
data Cmd = CQuit
         | CHelp
         | CRun
         | CC
         | CS
         | CInfoReg
         | CInfoB
         | CDisasAd Int
         | CDisasCr
         | CX Int Int
         | CPw Int Int
         | CDelete Int
         | CEnable Int
         | CDisable Int
         | CB Int
         | CWatchPc DbgOrd Int
         | CWatchGReg GReg DbgOrd Int
         | CWatchDmem Int DbgOrd Int
         | CBlank
         deriving (Show, Eq)

-- commands
type ParseCmd = Parser Cmd

line :: ParseCmd
line = do skipSpaces
          a <- command
          skipSpaces
          endOfInput
          return a

-- each command
command :: ParseCmd
command = cmdQuit <|> cmdHelp <|> cmdRun <|> cmdS <|> cmdC
      <|> cmdInfoReg <|> cmdInfoB
      <|> cmdDisasAd <|> cmdDisasCr
      <|> cmdXn <|> cmdX1 <|> cmdPw <|> cmdPr
      <|> cmdDelete <|> cmdEnable <|> cmdDisable
      <|> cmdB
      <|> cmdWatchPc <|> cmdWatchGReg <|> cmdWatchDmem
      <|> cmdBlank

-- blank
cmdBlank :: ParseCmd
cmdBlank = CBlank <$ skipSpaces

-- run
cmdQuit, cmdHelp, cmdRun, cmdC, cmdS :: ParseCmd
cmdQuit = CQuit <$ string "q"
cmdHelp = CHelp <$ string "help"
cmdRun  = CRun  <$ string "run"
cmdS    = CS    <$ string "s"
cmdC    = CC    <$ string "c"

cmdInfoReg, cmdInfoB :: ParseCmd
cmdInfoReg = CInfoReg <$ (string "info" >> delimSpace >> string "reg")
cmdInfoB   = CInfoB   <$ (string "info" >> delimSpace >> string "b")

-- disassemble
cmdDisasAd, cmdDisasCr :: ParseCmd
cmdDisasAd = CDisasAd <$> (string "disas" >> delimSpace *> num)
cmdDisasCr = CDisasCr <$   string "disas"

-- memory access
cmdXn, cmdX1, cmdPr, cmdPw :: ParseCmd
cmdXn = CX   <$> (string "x/" >> num) <*> (delimSpace >> num)
cmdX1 = CX 1 <$> (string "x" >> delimSpace *> num)
cmdPr = CX 1 <$> (string "p" >> delimSpace >> string "*" >> num)
cmdPw = CPw  <$> (string "p" >> delimSpace >> string "*" >> num)
             <*> (skipSpaces >> string "=" >> skipSpaces >> num)

-- break utility
cmdDelete, cmdEnable, cmdDisable :: ParseCmd
cmdDelete  = CDelete  <$> (string "delete"  >> delimSpace *> num)
cmdEnable  = CEnable  <$> (string "enable"  >> delimSpace *> num)
cmdDisable = CDisable <$> (string "disable" >> delimSpace *> num)


-- break and watch
cmdB :: ParseCmd
cmdB = CB <$> (string "b" >> delimSpace *> num)

cmdWatchPc :: ParseCmd
cmdWatchPc = CWatchPc
               <$> (string "watch" >> delimSpace >> string "pc" >>
                    skipSpaces >> dbgord)
               <*> (skipSpaces >> num)

cmdWatchGReg :: ParseCmd
cmdWatchGReg = CWatchGReg
                 <$> (string "watch" >> delimSpace >> greg)
                 <*> (skipSpaces >> dbgord)
                 <*> (skipSpaces >> num)

cmdWatchDmem :: ParseCmd
cmdWatchDmem = CWatchDmem
                 <$> (string "watch" >> delimSpace >> string "*" >> num)
                 <*> (skipSpaces >> dbgord)
                 <*> (skipSpaces >> num)

-- utility
skipSpaces :: Parser ()
skipSpaces = skipWhile P8.isHorizontalSpace

delimSpace :: Parser ()
delimSpace = satisfy P8.isHorizontalSpace *> skipWhile P8.isHorizontalSpace


-- number
num :: Parser Int
num = numMinus <|> numHex <|> numNoSign

numNoSign :: Parser Int
numNoSign = do d <- P.takeWhile1 (inClass "0123456789")
               return $ read (B.unpack d)

numMinus :: Parser Int
numMinus = do P8.char8 '-'
              d <- P.takeWhile1 (inClass "0123456789")
              return $ read ('-' : B.unpack d)

numHex :: Parser Int
numHex = do string "0x"
            d <- P.takeWhile1 (inClass "0123456789abcdef")
            return $ read ("0x" ++ B.unpack d)


-- DbgOrd
dbgord :: Parser DbgOrd
dbgord = do a <- choice $ map string [ "==" , "!=" , "<=" , "<" , ">=" , ">" ]
            return $ strToDbgOrd a

strToDbgOrd :: B.ByteString -> DbgOrd
strToDbgOrd "==" = BEQ
strToDbgOrd "!=" = BNE
strToDbgOrd "<"  = BLT
strToDbgOrd "<=" = BLE
strToDbgOrd ">"  = BGT
strToDbgOrd ">=" = BGE
strToDbgOrd x    = error $ "strToDbgOrd" ++ (show x)


-- GReg
greg :: Parser GReg
greg = do let reverseSortedGregNames = sortBy (flip compare) gregNames
          a <- choice $ map string reverseSortedGregNames
          return $ strToGReg a

gregNames :: [B.ByteString]
gregNames  = map (B.pack . (map toLower) . show)
               [(minBound :: GReg) .. (maxBound :: GReg)]

strToGReg :: B.ByteString -> GReg
strToGReg  x = case (elemIndex x gregNames) of
                 Just n  -> toEnum n
                 Nothing -> error $ "strToGReg" ++ (show x)




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



