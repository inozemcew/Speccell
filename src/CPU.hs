-- CPU.hs
module CPU
    ( cpuStep
    ) where

import Machine
import Control.Monad.IO.Class

cpuStep :: (MonadIO m) => Machine -> m (Machine, Int)
cpuStep m0 = do
  -- пока заглушка
    let cpu = mCPU m0
        (b,t) = (1,4) -- command length in (bytes,t-states)
        m = m0
            { mCPU = cpu
                { cpuPC = cpuPC cpu + b }
            , mTStates = mTStates m0 + t
            }
    return (m, t)

