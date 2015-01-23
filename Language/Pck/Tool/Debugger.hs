
{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}


module Language.Pck.Tool.Debugger (
        -- * Debugger driver
          runDbg
        , runDbgIO
        , evalProgDbg
        -- * Data type
        -- ** trace
        , TrcLog
        , DbgTrc(..)
        -- ** break
        , DbgBrk(..)
        , DbgOrd(..)
  )where

import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Text.Printf (printf)
import Data.List (intercalate)

import Language.Pck.Cpu.Instruction
import Language.Pck.Cpu.Memory
import Language.Pck.Cpu.Register
import Language.Pck.Cpu.State
import Language.Pck.Cpu.Execution



----------------------------------------
--  driver
----------------------------------------
-- | debugging run
--
-- Example: run with break condition. (break at pc == 1)
--
-- >  > runDbg [] [(BrkPc BEQ 1)] [(0,[MOVI R0 7, MOVI R1 8, HALT])] []
-- >  pc : 1
-- >  gr : [7,0,0,0,0,0,0,0]
-- >  fl : [False,False]
--
--
-- Example: run with trace output. (instruction trace)
--
-- >  > runDbg [TrcInst] [] [(0,[MOVI R0 7, MOVI R1 8, HALT])] []
-- >  TrcInst:        pc : 0  MOVI R0 7
-- >  
-- >  TrcInst:        pc : 1  MOVI R1 8
-- >  
-- >  TrcInst:        pc : 2  HALT
--
runDbg :: [DbgTrc] -> [DbgBrk] -> InstImage -> DataImage -> (TrcLog, CpuState)
runDbg dbgtrc dbgbrk insts vals = runState (evalProgDbg dbgtrc dbgbrk)
                                    (initCpuStateMem insts vals)


-- | debugging run for IO stdout
--
-- Example: run with trace output. (instruction trace)
--
-- >  > runDbgIO [TrcInst] [] [(0,[MOVI R0 7, MOVI R1 8, HALT])] []
-- >  TrcInst:        pc : 0  MOVI R0 7
-- >  
-- >  TrcInst:        pc : 1  MOVI R1 8
-- >  
-- >  TrcInst:        pc : 2  HALT
--
runDbgIO :: [DbgTrc] -> [DbgBrk] -> InstImage -> DataImage -> IO ()
runDbgIO dbgtrc dbgbrk insts vals =
    let (trc, _) = runDbg dbgtrc dbgbrk insts vals
    in  putStr $ B.unpack trc

-- eval prog
evalProgDbg :: [DbgTrc] -> [DbgBrk] -> EvalCpu TrcLog
evalProgDbg dbgtrc dbgbrk = loop B.empty 0
    where loop trclog cnt = do trclog' <- tracePre dbgtrc trclog
                               res  <- evalProg True
                               res' <- checkBreak dbgbrk res
                               trclog'' <- tracePost dbgtrc trclog'
                               checkRunLimit cnt
                               case res' of
                                 RsNormal -> loop trclog'' (cnt+1)
                                 _        -> return trclog''

-- run limiter for inf loop
dbgRunLimit :: Int
dbgRunLimit = 1000000

checkRunLimit :: Int -> EvalCpu ()
checkRunLimit n
    | n < dbgRunLimit = return ()
    | otherwise       = do s <- get
                           error $ "RUN COUNT OVER!\n" ++ show s


----------------------------------------
--  debug trace
----------------------------------------
-- | data type for 'runDbg' output log
type TrcLog = B.ByteString

-- | trace conditions for 'runDbg' or 'runDbgIO'
data DbgTrc = TrcInst   -- ^ trace instructions
            | TrcReg    -- ^ trace registers
            | TrcPc     -- ^ trace pc
            | TrcCall   -- ^ trace call target address
            | TrcBranch -- ^ trace branch information
            | TrcLoad   -- ^ trace memory load
            | TrcStore  -- ^ trace memory store
            deriving (Show, Eq)

-- pre/post trace
tracePre, tracePost :: [DbgTrc] -> TrcLog -> EvalCpu TrcLog
tracePre  = traceMany [TrcInst, TrcPc, TrcBranch, TrcCall, TrcLoad, TrcStore]
tracePost = traceMany [TrcReg]

traceMany :: [DbgTrc] -> [DbgTrc] -> TrcLog -> EvalCpu TrcLog
traceMany target dbgtrc trclog = do let list = filter (`elem` target) dbgtrc
                                    l <- mapM traceOne list
                                    return $ B.append trclog (B.concat l)

traceOne :: DbgTrc -> EvalCpu TrcLog
traceOne TrcPc     = tracePc
traceOne TrcInst   = traceInst
traceOne TrcReg    = traceReg
traceOne TrcCall   = traceCall
traceOne TrcBranch = traceBranch
traceOne TrcLoad   = traceLoad
traceOne TrcStore  = traceStore


-- each trace
tracePc :: EvalCpu TrcLog
tracePc = do pc <- readPc
             return $ B.pack $ concat ["TrcPc:\tpc : ", (pprHex pc), "\n"]

traceInst :: EvalCpu TrcLog
traceInst = do pc <- readPc
               inst <- fetchInst
               return $ B.pack $ concat [ "TrcInst:\tpc : ",  (pprHex pc), "\t"
                                        , (show inst), "\n\n"]

traceReg :: EvalCpu TrcLog
traceReg = do stat <- get
              return $ B.pack $ concat
                    [ "TrcReg:\n"
                    , "pc : ",  pprHex (pcFromCpuState stat)
                    , "\ngr : ", pprHexList (grFromCpuState stat)
                    , "\nfl : ", show (flFromCpuState stat), "\n\n"]


traceLoad :: EvalCpu TrcLog
traceLoad = traceAddress isLoadInst "TrcLoad:\tload-ad : "

traceStore :: EvalCpu TrcLog
traceStore = traceAddress isStoreInst "TrcStore:\tstore-ad : "

traceCall :: EvalCpu TrcLog
traceCall = traceAddress isCallInst "TrcCall:\ttarget : "

traceAddress :: (Inst -> Maybe GReg) -> String -> EvalCpu TrcLog
traceAddress prd str =  do pc <- readPc
                           inst <- fetchInst
                           case (prd inst) of
                             Just reg -> do ad <- readGReg reg
                                            return $ pprSIIInst str ad pc inst
                             _        -> return ""

isLoadInst :: Inst -> Maybe GReg
isLoadInst (LD _ reg) = Just reg
isLoadInst _          = Nothing

isStoreInst :: Inst -> Maybe GReg
isStoreInst (ST reg _) = Just reg
isStoreInst _          = Nothing

isCallInst :: Inst -> Maybe GReg
isCallInst (CALL reg) = Just reg
isCallInst _          = Nothing


traceBranch :: EvalCpu TrcLog
traceBranch = do pc <- readPc
                 inst <- fetchInst
                 case inst of
                   BRI cond imm -> do flag <- readFlags
                                      let strTaken = if (judgeFCond flag cond)
                                                       then "Taken" else "Not"
                                      return $ pprTrcBranch
                                                 (pc+imm) strTaken pc inst

                   JRI      imm -> return $ pprTrcBranch
                                              (pc+imm) "Taken" pc inst

                   J        reg -> do ad <- readGReg reg
                                      return $ pprTrcBranch ad "Taken" pc inst

                   CALL     reg -> do ad <- readGReg reg
                                      return $ pprTrcBranch ad "Taken" pc inst

                   _            -> return ""

-- pretty print utility
pprHex :: Int -> String
pprHex = printf "0x%x"

pprHexList :: [Int] -> String
pprHexList xs = "[" ++ (intercalate "," (map pprHex xs)) ++ "]"

pprSIIInst :: String -> Int -> Int -> Inst -> TrcLog
pprSIIInst str n pc inst = B.pack $ concat
                             [ str, (show n), "\t -- "
                             , "pc : " , (show pc), "\t"
                             , (show inst), "\n\n" ]

pprTrcBranch :: Int -> String -> Int -> Inst -> TrcLog
pprTrcBranch ad str pc inst = B.pack $ concat
                                [ "TrcBranch:\ttarget : ", (show ad), "\t"
                                , str, "\t -- "
                                , "pc : ", (show pc), "\t"
                                , (show inst) , "\n\n" ]


----------------------------------------
--  debug break
----------------------------------------

-- | break conditions
--
-- Example:
--
-- >  BrkPc BEQ 3          -- pc == 3
-- >  BrkPc BGE 0x80       -- pc >= 0x80
-- >  BrkGReg R0 BEQ 7     -- R0 == 7
-- >  BrkDmem 0x20 BLT 4   -- *0x20 < 4
--
data DbgBrk = BrkNon                  -- ^ no break
            | BrkOne                  -- ^ always one step break
            | BrkPc   DbgOrd Int      -- ^ pc break
            | BrkGReg GReg DbgOrd Int -- ^ register break
            | BrkDmem Int DbgOrd Int  -- ^ data memory break
            deriving (Eq)

-- | break operators
data DbgOrd = BEQ  -- ^ equal
            | BNE  -- ^ not equal
            | BLT  -- ^ little than
            | BLE  -- ^ little equal
            | BGT  -- ^ greater than
            | BGE  -- ^ greater equal
            deriving (Eq, Show)



-- user break setting 
checkBreak :: [DbgBrk] -> ResultStat -> EvalCpu ResultStat
checkBreak [] res    = return res
checkBreak dbgbrk res = do b <- mapM breakOne dbgbrk
                           return $ if (RsDbgBrk `elem` b)
                                      then RsDbgBrk else res

breakOne :: DbgBrk -> EvalCpu ResultStat
breakOne (BrkNon)   = return RsNormal
breakOne (BrkOne)   = return RsDbgBrk

breakOne (BrkPc o v) = do pc <- readPc
                          return $ if (ordFunc o) pc v
                                     then RsDbgBrk else RsNormal

breakOne (BrkGReg reg o v) = do reg' <- readGReg reg
                                return $ if (ordFunc o) reg' v
                                           then RsDbgBrk else RsNormal

breakOne (BrkDmem ad o v) = do mem <- readDmem ad
                               return $ if (ordFunc o) mem v
                                          then RsDbgBrk else RsNormal

ordFunc :: DbgOrd -> (Int -> Int -> Bool)
ordFunc BEQ = (==)
ordFunc BNE = (/=)
ordFunc BLT = (<)
ordFunc BLE = (<=)
ordFunc BGT = (>)
ordFunc BGE = (>=)

