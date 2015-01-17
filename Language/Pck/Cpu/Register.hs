
{-# OPTIONS -Wall #-}


module Language.Pck.Cpu.Register (
        -- * Note
        -- $note

        -- * Basic type
          GRegArray
        , Flag(..)
        , FlagArray
        -- * general purpose register access
        , initGReg
        , getGReg
        , getGReg2
        , getGRegs
        , modifyGReg
        -- * flag register access
        , initFlag
        , getFlag
        , getFlags
        , modifyFlag
        , judgeFCond
  ) where

import Data.Array (Array, Ix,  listArray, (//), (!), elems)

import Language.Pck.Cpu.Instruction

-- $note
-- This is implementation dependent module.
-- It's better to use Language.Pck.Cpu.State functions.

----------------------------------------
-- general purpose register implementation
----------------------------------------
type GRegArray = Array GReg Int

-- | initialize general purpose register array
initGReg :: GRegArray
initGReg = listArray (minBound::GReg, maxBound::GReg)
             $ replicate (fromEnum (maxBound::GReg) + 1) 0

-- | get general purpose register value
getGReg :: GRegArray -> GReg -> Int
getGReg ary reg = ary ! reg

-- | get general purpose register pair value
getGReg2 :: GRegArray -> GReg -> GReg -> (Int, Int)
getGReg2 ary ra rb = (ary ! ra, ary ! rb)

-- | get all general purpose registers
getGRegs :: GRegArray -> [Int]
getGRegs = elems

-- | modify general purpose registers
modifyGReg :: GRegArray -> GReg -> Int -> GRegArray
modifyGReg ary reg val = ary // [(reg,val)]

----------------------------------------
-- flag register implementation
----------------------------------------
data Flag = FLZ | FLC
              deriving (Show, Eq, Ord, Ix, Enum, Bounded)

type FlagArray = Array Flag Bool

-- | initialize flag register
initFlag :: FlagArray
initFlag = listArray (minBound::Flag, maxBound::Flag)
             $ replicate (fromEnum (maxBound::Flag) + 1) False

-- | get flag register value
getFlag :: FlagArray -> Flag -> Bool
getFlag ary flag = ary ! flag

-- | get all flag register values
getFlags :: FlagArray -> [Bool]
getFlags = elems

-- | modify flag registers
modifyFlag :: FlagArray -> Flag -> Bool -> FlagArray
modifyFlag ary flag val = ary // [(flag,val)]


-- | judge flag condition
judgeFCond :: FlagArray -> FCond -> Bool
judgeFCond ary FCEQ = (getFlag ary FLZ) == True
judgeFCond ary FCNE = (getFlag ary FLZ) == False
judgeFCond ary FCLT = (getFlag ary FLC) == True
judgeFCond ary FCLE = (getFlag ary FLC) == True  || (getFlag ary FLZ) == True
judgeFCond ary FCGT = (getFlag ary FLC) == False && (getFlag ary FLZ) == False
judgeFCond ary FCGE = (getFlag ary FLC) == False

