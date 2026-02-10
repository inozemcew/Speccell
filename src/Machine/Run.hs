module Machine.Run
    (  cpuStep
    ) where

import Machine
import Machine.CPU

import Data.Bits
import Control.Monad.IO.Class
import Numeric(showHex)

type StepResult = IO (Machine, Address, Int)
stubResult :: Machine ->  StepResult
stubResult machine = return (machine, 1, 4)

cpuStep :: (MonadIO m) => Machine -> m (Machine, Int)
cpuStep machine = liftIO $ do
    let op = cpuOP $ mCPU machine
        pc = cpuPC $ mCPU machine
    opcode <- readMemory machine pc
    print (mCPU machine)
    print $ showHex opcode ""
    -- (new machine state, bytes, t-states)
    (m1,b,t) <- case op of
        CBPrefix -> cbInstructions machine opcode
        EDPrefix -> edInstructions machine opcode
        _Prefix  -> ordInstructions machine opcode
    let cpu = mCPU m1
    let m2 = m1
            { mCPU = cpu
                { cpuPC = cpuPC cpu + b
                , cpuR  = cpuR  cpu + 1 }
            , mTStates = mTStates machine + t
            }
    return (m2, t)

ordInstructions :: Machine -> Byte -> StepResult
ordInstructions machine opcode = case opcode .&. 0xC0 of
    0x00 -> q00
    0x40 -> q40
    0x80 -> q80
    0xc0 -> qC0
    _Err -> error "Strange opcode"
    where
        q00 = case opcode .&. 0x0F of
            0x00 -> instX0 machine opcode
            0x01 -> instLD_RR_W machine opcode
            0x02 -> instLD_mem_R machine opcode
            0x03 -> instINC_RR machine (opcode `shiftR` 4) False
            0x04 -> instINC_DEC_R machine (opcode `shiftR` 3) False
            
            0x0B -> instINC_RR machine (opcode `shiftR` 4) True
            0x0C -> instINC_DEC_R machine (opcode `shiftR` 3) True
            _    -> stubResult machine
        q40 = if opcode == 0x76
            then instHalt machine
            else instLD_R_R machine (shiftR opcode 3 .&. 7) (opcode .&. 7)
        q80 = stubResult machine
        qC0 = stubResult machine


instX0 :: Machine -> Byte -> StepResult
instX0 machine opcode = stubResult machine

---------------------------
-- LD RR,nn instructions --
---------------------------
instLD_RR_W :: Machine -> Byte -> StepResult
instLD_RR_W machine opcode = do
    let cpu = mCPU machine
    w <- readMemoryW machine (cpuPC cpu + 1)
    let cpu' = case opcode .&. 0x30 of
                    0x00 -> setBC cpu w
                    0x10 -> setDE cpu w
                    0x20 -> setHL_IX_IY cpu w
                    0x30 -> setSP cpu w
                    _    -> error "Unknown register in LD RR,nn"
    return (machine{mCPU = cpu'}, 3, 10)


---------------------------------------------------------
-- LD (BC\DE\mem),a and LD (mem),HL\IX\IY instructions --
---------------------------------------------------------
instLD_mem_R :: Machine -> Byte -> StepResult
instLD_mem_R machine opcode = do
    let cpu = mCPU machine
    if opcode == 0x22 
        then do
            addr <- readMemoryW machine $ cpuPC cpu + 1
            writeMemoryW machine addr $ getHL_IX_IY cpu
            return (machine, 3, 16)
        else do
            (addr, b, t) <- case opcode of
                0x02 -> return (getBC cpu, 1, 7) 
                0x12 -> return (getDE cpu, 1, 7)
                0x32 -> do 
                    addr <- readMemoryW machine $ cpuPC cpu + 1
                    return (addr, 3, 13)
                _   -> error "Unknown register in LD (mem),nn"
            writeMemory machine addr $ cpuA cpu
            return (machine, b, t)


-------------------------
-- INC rr instructions --
-------------------------
instINC_RR :: Machine -> Byte -> Bool -> StepResult
instINC_RR machine reg dec = do
    let v = if dec then 0xFFFF else 1
    let cpu = mCPU machine
    let cpu' = case reg of
                0 -> setBC cpu (getBC cpu + v)
                1 -> setDE cpu (getDE cpu + v)
                2 -> setHL_IX_IY cpu (getHL_IX_IY cpu + v)
                3 -> setSP cpu (getSP cpu + v)
                _ -> error "inc XX reg>3"
    return (machine{mCPU = cpu'}, 1, 6)


----------------------------
-- INC\DEC r instructions --
----------------------------
instINC_DEC_R :: Machine -> Byte -> Bool -> StepResult
instINC_DEC_R machine reg dec = do
    let cpu = mCPU machine
        op = cpuOP cpu
    if reg == 6
    then do
        error "INC (HL) not implemented"
        --return (machine,1,7)
    else do
        let r = getReg8 cpu op reg
        let (r', upd) = if dec 
                then (r - 1, updFlagsDec)
                else (r + 1, updFlagsInc)
            cpu' = upd (setReg8 cpu op reg r') r r'
        return (machine{mCPU = cpu'}, 1, 4)


-------------------------
-- LD r,r instructions --
-------------------------
instLD_R_R :: Machine -> Byte -> Byte -> StepResult
instLD_R_R machine r1 r2
    | r1 == 6 = do   -- ld (hl),r
        (addr, b, t) <- forIXIY
        writeMemory machine addr $ getReg8 cpu NoPrefix r2
        return (machine, b, t)
    | r2 == 6 = do          -- ld r,(hl)
        (addr, b, t) <- forIXIY
        cpu' <- setReg8 cpu NoPrefix r1 <$> readMemory machine addr
        return (machine{mCPU = cpu'}, b, t)
    | otherwise = return (machine{mCPU = setReg8 cpu op r1 $ getReg8 cpu op r2}, 1, 4)
    where
        cpu = mCPU machine
        op = cpuOP cpu
        forIXIY = case op of
                    DDPrefix -> do
                        o <- readMemory machine (cpuPC cpu + 1)
                        return (getIX_Off cpu o, 2, 15)
                    FDPrefix -> do
                        o <- readMemory machine (cpuPC cpu + 1)
                        return (getIY_Off cpu o, 2, 15)
                    _OtherPrefix ->
                        return (getHL cpu, 1, 7)


instHalt :: Machine -> StepResult
instHalt machine = error "Halt - not implemented yet"
                   --return (machine, 0, 4)

cbInstructions :: Machine -> Byte -> StepResult
cbInstructions machine opcode = do
    stubResult machine

edInstructions :: Machine -> Byte -> StepResult
edInstructions machine opcode = do
    stubResult machine


