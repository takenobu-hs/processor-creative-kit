
{-# OPTIONS -Wall #-}


module Language.Pck.Cpu.State (
        -- * Evaluation monad (State monad)
        EvalCpu
        -- * Cpu state type
      , CpuState
      , pcFromCpuState
      , grFromCpuState
      , flFromCpuState
      , imemFromCpuState
      , dmemFromCpuState
      , dumpCpuState
      , initCpuState
      , initCpuStateMem
        -- * Result type
      , ResultStat(..)
        -- * access for the Cpu state
        -- ** PC(program counter)
      , readPc
      , updatePc
      , incPc
        -- ** General purpose registers
      , readGReg
      , readGReg2
      , updateGReg
        -- ** Flags
      , readFlags
      , updateFlag
        -- ** Instruction memory
      , fetchInst
        -- ** Data memory
      , readDmem
      , updateDmem
  ) where

import Control.Monad.State

import Language.Pck.Cpu.Config
import Language.Pck.Cpu.Instruction
import Language.Pck.Cpu.Register
import Language.Pck.Cpu.Memory


----------------------------------------
--  cpu state monad
----------------------------------------
-- | the cpu eval monad
type EvalCpu a = State CpuState a

-- | the result state
data ResultStat = RsNormal      -- ^ normal result
                | RsHalt        -- ^ cpu halt(stop)
                | RsDbgBrk      -- ^ debugger triggered
                | RsErr String  -- ^ execution error
                deriving (Show, Eq)



----------------------------------------
--  cpu state type
----------------------------------------
-- | the cpu state (processor internal state)
--
--   This is the result type from 'Language.Pck.Cpu.run' function.
--
--   get each values by 'pcFromCpuState', 'grFromCpuState', 'flFromCpuState',
--     'imemFromCpuState', 'dmemFromCpuState', 'dumpCpuState'
data CpuState = CpuState
                { state_pc :: Int
                , state_gr :: GRegArray
                , state_fl :: FlagArray
                , state_imem :: ImemArray
                , state_dmem :: DmemArray
                }
                deriving Eq

-- accessor

-- |
-- >  > pcFromCpuState $ run [(0,[MOVI R0 7, HALT])] []
-- >  1
--
pcFromCpuState   :: CpuState -> Int
pcFromCpuState   = state_pc

-- |
-- >  > grFromCpuState $ run [(0,[MOVI R0 7, HALT])] []
-- >  [7,0,0,0,0,0,0,0]
--
grFromCpuState   :: CpuState -> [Int]
grFromCpuState   = getGRegs . state_gr

-- |
-- >  > flFromCpuState $ run [(0,[MOVI R0 7, HALT])] []
-- >  [False,False]
--
flFromCpuState   :: CpuState -> [Bool]
flFromCpuState   = getFlags . state_fl

-- |
-- >  > imemFromCpuState $ run [(0,[MOVI R0 7, HALT])] []
-- >  [(0,[MOVI R0 7,HALT,UNDEF,UNDEF,...])]
--
imemFromCpuState :: CpuState -> InstImage
imemFromCpuState = getInstImage . state_imem

-- |
-- >  > dmemFromCpuState $ run [(0,[MOVI R0 0, MOVI R1 10, ST R0 R1, HALT])] []
-- >  [(0,[10,0,0,0,0,...])]
--
dmemFromCpuState :: CpuState -> DataImage
dmemFromCpuState = getDataImage . state_dmem


-- show
instance Show CpuState where
  show = showCpuState

showCpuState :: CpuState -> String
showCpuState s =
  "pc : " ++ show (pcFromCpuState s) ++
  "\ngr : " ++ show (grFromCpuState s) ++
  "\nfl : " ++ show (flFromCpuState s) ++
  "\nim : " ++ show (imemFromCpuState s) ++
  "\ndm : " ++ show (dmemFromCpuState s) ++
  "\n"

-- | dump Cpu state (without instruction image)
--
-- >  > putStr $ dumpCpuState $ run [(0,[MOVI R0 7, HALT])] []
-- >  pc : 1
-- >  gr : [7,0,0,0,0,0,0,0]
-- >  fl : [False,False]
-- >  dm : [(0,[7,0,0,0,0,...])]
--
dumpCpuState :: CpuState -> String
dumpCpuState s =
  "pc : " ++ show (pcFromCpuState s) ++
  "\ngr : " ++ show (grFromCpuState s) ++
  "\nfl : " ++ show (flFromCpuState s) ++
  "\ndm : " ++ show (dmemFromCpuState s) ++
  "\n"


----------------------------------------
--  initial state
----------------------------------------
-- | a default CpuState
initCpuState :: CpuState
initCpuState = CpuState
                 { state_pc = cfgStartPc cpuConfig
                 , state_gr = initGReg
                 , state_fl = initFlag
                 , state_imem = initImem 
                 , state_dmem = initDmem
                 }

-- | initialize CpuState by inst and data image
initCpuStateMem :: InstImage -> DataImage -> CpuState
initCpuStateMem insts vals = initCpuState {state_imem = presetImem insts
                                          ,state_dmem = presetDmem vals}


----------------------------------------
--  state utility
----------------------------------------
-- | read the pc

readPc :: EvalCpu Int
readPc = gets state_pc

-- | update the pc
--
-- Example:
--
-- >  jumpRI :: Int -> EvalCpu ResultStat
-- >  jumpRI ad = do pc <- readPc
-- >                 updatePc (pc + ad)
--
updatePc :: Int -> EvalCpu ResultStat
updatePc pc = do modify $ \s -> s { state_pc = pc }
                 return RsNormal

-- | increment the pc
incPc :: EvalCpu ResultStat
incPc = do pc <- readPc
           updatePc (pc + 1)


-- | read a general purpose register
--
-- Example:
--
-- >  jump :: GReg -> EvalCpu ResultStat
-- >  jump reg = do ad <- readGReg reg
-- >                updatePc ad
--
readGReg :: GReg -> EvalCpu Int
readGReg ra = do gr <- gets state_gr
                 return $ getGReg gr ra

-- | read general purpose register pair
readGReg2 :: GReg -> GReg -> EvalCpu (Int, Int)
readGReg2 ra rb = do gr <- gets state_gr
                     return (getGReg gr ra, getGReg gr rb)

-- | update a general purpose register
--
-- Example:
--
-- >  movpc :: GReg -> EvalCpu ResultStat
-- >  movpc reg = do pc <- readPc
-- >                 updateGReg reg pc
--
updateGReg :: GReg -> Int -> EvalCpu ()
updateGReg reg val = do cpu <- get
                        let gr' = modifyGReg (state_gr cpu) reg val
                        put cpu { state_gr = gr' }


-- | read flag registers
--
-- Example:
--
-- >  branchRI :: FCond -> Int -> EvalCpu ResultStat
-- >  branchRI fcond ad  = do flags <- readFlags
-- >                          if judgeFCond flags fcond
-- >                             then jumpRI ad
-- >                             else incPc
--
readFlags :: EvalCpu FlagArray
readFlags = gets state_fl

-- | update a flag
--
-- Example:
--
-- >  cmpRR :: GReg -> GReg -> EvalCpu ResultStat
-- >  cmpRR ra rb = do (ra', rb') <- readGReg2 ra rb
-- >                   updateFlag FLZ (ra' == rb')
-- >                   updateFlag FLC (ra' <  rb')
--
updateFlag :: Flag -> Bool -> EvalCpu ()
updateFlag flag val = do cpu <- get
                         let flags = state_fl cpu
                             fl'  = modifyFlag flags flag val
                         put cpu { state_fl = fl' }


-- | fetch an instruction from the instruction memory
fetchInst :: EvalCpu Inst
fetchInst = do imem <- gets state_imem
               pc <- readPc
               return $ fetchImem imem pc


-- | read a data value from the data memory
--
-- Example:
--
-- >  load :: GReg -> GReg -> EvalCpu ResultStat
-- >  load ra rb = do rb' <- readGReg rb
-- >                  ra' <- readDmem rb'
-- >                  updateGReg ra ra'
--
readDmem :: Int -> EvalCpu Int
readDmem ad = do cpu <- get
                 return $ getDmem (state_dmem cpu) ad

-- | update the data memory
--
-- Example:
--
-- >  store :: GReg -> GReg -> EvalCpu ResultStat
-- >  store ra rb = do (ra', rb') <- readGReg2 ra rb
-- >                   updateDmem ra' rb' 
--
updateDmem :: Int -> Int -> EvalCpu ()
updateDmem ad val = do cpu <- get
                       let dmem' = modifyDmem (state_dmem cpu) ad val
                       put cpu { state_dmem = dmem' }



