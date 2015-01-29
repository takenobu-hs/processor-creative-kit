
{-# OPTIONS -Wall #-}


module Language.Pck.Cpu.Register (
        -- * Note
        -- $note

        -- * Basic types
          GRegArray
        , Flag(..)
        , FlagArray
        -- * access to general purpose registers
        , initGReg
        , getGReg
        , getGReg2
        , getGRegs
        , modifyGReg
        -- * access to flag registers
        , initFlag
        , getFlag
        , getFlags
        , modifyFlag
        , judgeFCond
  ) where

import Data.Array (Array, Ix,  listArray, (//), (!), elems)

import Language.Pck.Cpu.Instruction

-- $note
-- This is an implementation dependent module.
-- It's better to use functions in Language.Pck.Cpu.State.

----------------------------------------
-- general purpose register implementation
----------------------------------------
type GRegArray = Array GReg Int

-- | initialize the general purpose registers array
initGReg :: GRegArray
initGReg = listArray (minBound::GReg, maxBound::GReg)
             $ replicate (fromEnum (maxBound::GReg) + 1) 0

-- | get a value of  the general purpose register
getGReg :: GRegArray -> GReg -> Int
getGReg ary reg = ary ! reg

-- | get values of the general purpose register pair
getGReg2 :: GRegArray -> GReg -> GReg -> (Int, Int)
getGReg2 ary ra rb = (ary ! ra, ary ! rb)

-- | get all values of the general purpose registers
getGRegs :: GRegArray -> [Int]
getGRegs = elems

-- | modify general purpose registers
modifyGReg :: GRegArray -> GReg -> Int -> GRegArray
modifyGReg ary reg val = ary // [(reg,val)]

----------------------------------------
-- flag register implementation
----------------------------------------
data Flag = FLZ  -- ^ zero flag
          | FLC  -- ^ carry flag
              deriving (Show, Eq, Ord, Ix, Enum, Bounded)

type FlagArray = Array Flag Bool

-- | initialize the flag registers array
initFlag :: FlagArray
initFlag = listArray (minBound::Flag, maxBound::Flag)
             $ replicate (fromEnum (maxBound::Flag) + 1) False

-- | get a value of the flag register value
getFlag :: FlagArray -> Flag -> Bool
getFlag ary flag = ary ! flag

-- | get all values of the flag registers
getFlags :: FlagArray -> [Bool]
getFlags = elems

-- | modify flag registers
modifyFlag :: FlagArray -> Flag -> Bool -> FlagArray
modifyFlag ary flag val = ary // [(flag,val)]


-- | judge a flag condition
judgeFCond :: FlagArray -> FCond -> Bool
judgeFCond ary FCEQ = (getFlag ary FLZ) == True
judgeFCond ary FCNE = (getFlag ary FLZ) == False
judgeFCond ary FCLT = (getFlag ary FLC) == True
judgeFCond ary FCLE = (getFlag ary FLC) == True  || (getFlag ary FLZ) == True
judgeFCond ary FCGT = (getFlag ary FLC) == False && (getFlag ary FLZ) == False
judgeFCond ary FCGE = (getFlag ary FLC) == False

