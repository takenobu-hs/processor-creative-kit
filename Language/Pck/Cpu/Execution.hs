
{-# OPTIONS -Wall #-}


module Language.Pck.Cpu.Execution (
        -- * Running processor
          run
        , evalProg
  ) where

import Control.Monad.State
import Data.Bits ((.&.), (.|.), complement, shift, xor)
import Data.Word

import Language.Pck.Cpu.Instruction
import Language.Pck.Cpu.Register
import Language.Pck.Cpu.Memory
import Language.Pck.Cpu.State


----------------------------------------
--  simulation driver
----------------------------------------
-- | run simulation driver
--
-- Example: simple run
--
-- >  > run [(0, [MOVI R0 20, HALT])] []
-- >  pc : 1
-- >  gr : [20,0,0,0,0,0,0,0]
-- >  fl : [False,False]
-- >  ...
--
-- Example: run with initial data
--
-- >  > run [(0, [MOVI R1 0, LD R0 R1, HALT])] [(0,[100])]
-- >  pc : 2
-- >  gr : [100,0,0,0,0,0,0,0]
-- >  fl : [False,False]
-- >  ...
--
run :: InstImage -> DataImage -> CpuState
run insts vals = execState (evalProg False) (initCpuStateMem insts vals)

-- | eval program
--
-- >
-- >  run :: InstImage -> DataImage -> CpuState
-- >  run insts vals = execState (evalProg False) (initCpuStateMem insts vals)
--
evalProg :: Bool -> EvalCpu ResultStat
evalProg isOneStep = loop 
    where loop = do inst <- fetchInst
                    res  <- evalStep inst
                    case res of
                      RsHalt  -> return res
                      RsErr e -> get >> error e
                      _       -> if isOneStep then return res else loop


----------------------------------------
--  instruction definition
----------------------------------------
evalStep :: Inst -> EvalCpu ResultStat
evalStep NOP              = incPc
evalStep HALT             = return RsHalt

evalStep (MOVI  reg imm)  = movimm reg imm
evalStep (MOV   ra rb)    = uniopInst (id) ra rb
evalStep (MOVPC ra)       = movpc ra

evalStep (ADD   ra rb rc) = biopInst (+) ra rb rc
evalStep (SUB   ra rb rc) = biopInst (-) ra rb rc
evalStep (CMP   ra rb)    = cmpRR ra rb
evalStep (ABS   ra rb)    = uniopInst (abs) ra rb
evalStep (ASH   ra rb rc) = biopInst (shift) ra rb rc
evalStep (MUL   ra rb rc) = biopInst (*) ra rb rc
evalStep (DIV   ra rb rc) = biopInst (div) ra rb rc

evalStep (AND   ra rb rc) = biopInst (.&.) ra rb rc
evalStep (OR    ra rb rc) = biopInst (.|.) ra rb rc
evalStep (NOT   ra rb)    = uniopInst (complement) ra rb
evalStep (XOR   ra rb rc) = biopInst (xor) ra rb rc
evalStep (LSH   ra rb rc) = biopInst (logicalShift) ra rb rc

evalStep (BRI   f ad)     = branchRI f ad
evalStep (JRI   ad)       = jumpRI ad
evalStep (J     reg)      = jump reg
evalStep (CALL  reg)      = call reg
evalStep (RET      )      = ret

evalStep (LD    ra rb)    = load ra rb
evalStep (ST    ra rb)    = store ra rb

evalStep UNDEF            = do pc <- readPc
                               return $ RsErr ("undefined instruction at pc = "
                                                 ++ show pc)

----------------------------------------
--  instruction behavior utility
----------------------------------------
-- jump and branch
jumpRI :: Int -> EvalCpu ResultStat
jumpRI ad = do pc <- readPc
               updatePc (pc + ad)

jump :: GReg -> EvalCpu ResultStat
jump reg = do ad <- readGReg reg
              updatePc ad

branchRI :: FCond -> Int -> EvalCpu ResultStat
branchRI fcond ad  = do flags <- readFlags
                        if judgeFCond flags fcond
                           then jumpRI ad
                           else incPc

call :: GReg -> EvalCpu ResultStat
call reg = do pc   <- readPc
              reg' <- readGReg reg
              updateGReg R0 (pc+1)
              updatePc reg'

ret :: EvalCpu ResultStat
ret = do reg <- readGReg R0
         updatePc reg


-- mov simple
movimm :: GReg -> Int -> EvalCpu ResultStat
movimm reg imm = do updateGReg reg imm
                    incPc

-- read pc
movpc :: GReg -> EvalCpu ResultStat
movpc reg = do pc <- readPc
               updateGReg reg pc
               incPc

-- load and store
load :: GReg -> GReg -> EvalCpu ResultStat
load ra rb = do rb' <- readGReg rb
                ra' <- readDmem rb'
                updateGReg ra ra'
                incPc

store :: GReg -> GReg -> EvalCpu ResultStat
store ra rb = do (ra', rb') <- readGReg2 ra rb
                 updateDmem ra' rb' 
                 incPc

-- arithmetic
cmpRR :: GReg -> GReg -> EvalCpu ResultStat
cmpRR ra rb = do (ra', rb') <- readGReg2 ra rb
                 updateFlag FLZ (ra' == rb')
                 updateFlag FLC (ra' <  rb')
                 incPc


-- operation
biopInst :: (Int -> Int -> Int) -> GReg -> GReg -> GReg -> EvalCpu ResultStat
biopInst op ra rb rc= do (rb', rc') <- readGReg2 rb rc
                         updateGReg ra (rb' `op` rc')
                         incPc

uniopInst :: (Int -> Int) -> GReg -> GReg -> EvalCpu ResultStat
uniopInst op ra rb = do rb' <- readGReg rb
                        updateGReg ra (op rb')
                        incPc

-- primitive operation
logicalShift :: Int -> Int -> Int
logicalShift val sft = fromIntegral $ toInteger $ 
                         (fromIntegral val :: Word32) `shift` sft


