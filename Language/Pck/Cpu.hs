
{-# OPTIONS -Wall #-}


module Processor.Core (
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

import Processor.Core.Instruction (Inst(..), GReg(..), FCond(..))
import Processor.Core.Memory (InstImage, DataImage)
import Processor.Core.State (CpuState,
                             pcFromCpuState, grFromCpuState, flFromCpuState,
                             imemFromCpuState, dmemFromCpuState,
                             dumpCpuState)
import Processor.Core.Execution (run)



