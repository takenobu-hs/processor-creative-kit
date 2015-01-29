
{-# OPTIONS -Wall #-}


module Language.Pck.Cpu.Config (
        -- * The configuration
          CpuConfig(..)
        , cpuConfig
  ) where


-- | the default configuration
--
--   You can change the configuration here.
cpuConfig :: CpuConfig
cpuConfig = CpuConfig
            { cfgStartPc = 0
            , cfgImemStart = 0
            , cfgImemSize  = 256
            , cfgDmemStart = 0
            , cfgDmemSize  = 256
            }


-- | the cpu configuration type
data CpuConfig = CpuConfig
                 { cfgStartPc   :: Int   -- ^ boot(start) pc
                 , cfgImemStart :: Int   -- ^ instruction memory start address
                 , cfgImemSize  :: Int   -- ^ instruction memory size
                 , cfgDmemStart :: Int   -- ^ data memory start address
                 , cfgDmemSize  :: Int   -- ^ data memory size
                 }


