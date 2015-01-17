
{-# OPTIONS -Wall #-}


module Language.Pck.Cpu (
        -- * Run processor
          run
        -- * Instruction set
        , Inst(..)
        , GReg(..)
        , FCond(..)
        -- * Instruction and data memory image
        , InstImage
        , DataImage
        -- * Cpu state (processor internal state)
        , CpuState
        , pcFromCpuState
        , grFromCpuState
        , flFromCpuState
        , imemFromCpuState
        , dmemFromCpuState
        , dumpCpuState
  ) where

import Language.Pck.Cpu.Instruction (Inst(..), GReg(..), FCond(..))
import Language.Pck.Cpu.Memory (InstImage, DataImage)
import Language.Pck.Cpu.State (CpuState,
                               pcFromCpuState, grFromCpuState, flFromCpuState,
                               imemFromCpuState, dmemFromCpuState,
                               dumpCpuState)
import Language.Pck.Cpu.Execution (run)



