
{-# OPTIONS -Wall #-}


module Processor.Core.Memory (
        -- * Abstraction Memory type
          InstImage
        , DataImage

        -- * Implementation dependent types and functions
        -- | It's better to use Processor.Core.State functions.

        -- ** internal types
        , ImemArray
        , DmemArray
        , IAddress
        , DAddress
        , DValue
        -- ** Instruction memory access
        , initImem
        , presetImem
        , modifyImems
        , fetchImem
        , getInstImage
        , extractImems
        -- ** Data memory access
        , initDmem
        , presetDmem
        , getDmem
        , modifyDmem
        , modifyDmems
        , getDataImage
        , extractDmems
  ) where

import Data.Array (Array, listArray, (//), (!), elems, assocs)

import Processor.Core.Config
import Processor.Core.Instruction


----------------------------------------
-- instruction memory implementation
----------------------------------------
type IAddress = Int

-- | instruction memory image
-- 
-- >  [(StartAddress, [Instruction1,  Instruction2, ...])]
-- 
-- Example:
-- 
-- >  [(0, [MOVI R1 0,  LD R0 R1,  HALT])]
-- 
type InstImage = [(IAddress, [Inst])]

-- | instruction memory array
type ImemArray = Array IAddress Inst

imemSize, imemMin, imemMax :: Int
imemSize = cfgImemSize  cpuConfig
imemMin  = cfgImemStart cpuConfig
imemMax  = imemMin + imemSize - 1

-- | initialize instruction memory
initImem :: ImemArray
initImem = listArray(imemMin, imemMax) $ replicate imemSize UNDEF

-- | preset instruction memory
presetImem :: InstImage -> ImemArray
presetImem = foldl modifyImems initImem

-- | modify instruction memory
modifyImems :: ImemArray -> (IAddress, [Inst]) -> ImemArray
modifyImems ary (start, insts) = ary // zip [start .. imemMax] insts

-- | fetch instruction from instruction memory
fetchImem :: ImemArray -> IAddress -> Inst
fetchImem ary ad = ary ! ad'
    where ad' = ad `rem` (imemMax + 1)     -- wrap address


-- | get instruction memory image
getInstImage :: ImemArray -> InstImage
getInstImage ary = [(ad, val)]
    where ary' = assocs ary
          ad = fst $ head ary'
          val = elems ary

-- TODO efficiency implement
-- | extract instructions from instruction memory
extractImems :: InstImage -> IAddress -> Int -> [Inst]
extractImems img ad cnt = take cnt $ drop beg vals
    where (start, vals):_ = img
          beg = ad - start



----------------------------------------
-- data memory implementation
----------------------------------------
type DAddress = Int
type DValue   = Int

-- | data memory image
-- 
-- >  [(StartAddress, [Data1,  Data2, ...])]
-- 
-- 
-- Example:
-- 
-- >  [(0, [1, 5, 7, 0x20])]
-- 
type DataImage = [(DAddress, [DValue])]

-- | data memory array
type DmemArray = Array DAddress DValue
dmemSize, dmemMin, dmemMax :: Int
dmemSize = cfgDmemSize  cpuConfig
dmemMin  = cfgDmemStart cpuConfig
dmemMax  = dmemMin + dmemSize - 1

-- | initialize data memory
initDmem :: DmemArray
initDmem = listArray (dmemMin,dmemMax) $ replicate dmemSize 0

-- | preset data memory
presetDmem :: DataImage -> DmemArray
presetDmem = foldl modifyDmems initDmem

-- | get data from data memory
getDmem :: DmemArray -> DAddress -> DValue
getDmem ary ad = ary ! ad'
    where ad' = ad `rem` (dmemMax + 1)     -- wrap address

-- | modify data memory
modifyDmem :: DmemArray -> DAddress -> DValue -> DmemArray
modifyDmem ary ad dat = ary // [(ad', dat)]
    where ad' = ad `rem` (dmemMax + 1)     -- wrap address

-- | modify data memory by values
modifyDmems :: DmemArray -> (DAddress, [DValue]) -> DmemArray
modifyDmems ary (start, vals) = ary // zip [start .. dmemMax] vals

-- | get data memory image
getDataImage :: DmemArray -> DataImage
getDataImage ary = [(ad, val)]
    where ary' = assocs ary
          ad = fst $ head ary'
          val = elems ary

-- TODO range check! and efficiency implement
-- | extract data values from data memory
extractDmems :: DataImage -> DAddress -> Int -> [DValue]
extractDmems img ad cnt = take cnt $ drop beg vals
    where (start, vals):_ = img
          beg = ad - start



