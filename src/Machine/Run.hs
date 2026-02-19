module Machine.Run
    (  cpuStep
    ) where

import Machine
import Machine.CPU
import Machine.CPUFlags

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
        m0 = machine{mCPU=(mCPU machine) {cpuOP2 = NoPrefix}}
    opcode <- readMemory machine pc
 -- (new machine state, bytes, t-states)
    (m1,b,t) <- case op of
        CBPrefix -> cbInstructions m0 opcode
        EDPrefix -> edInstructions m0 opcode
        _Prefix  -> ordInstructions m0 opcode
    let cpu = mCPU m1
    let m2 = m1
            { mCPU = cpu
                { cpuPC = cpuPC cpu + b
                , cpuR  = cpuR  cpu + 1 
                , cpuOP = cpuOP2 cpu}
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
            0x05 -> instINC_DEC_R machine (opcode `shiftR` 3) True
            0x06 -> instLD_R_n machine (opcode `shiftR` 3)
            
            0x08 -> instX0 machine opcode
            0x09 -> instADD_HL_RR machine opcode
            0x0A -> instLD_R_mem machine opcode
            0x0B -> instINC_RR machine (opcode `shiftR` 4) True
            0x0C -> instINC_DEC_R machine (opcode `shiftR` 3) False
            0x0D -> instINC_DEC_R machine (opcode `shiftR` 3) True
            0x0E -> instLD_R_n machine (opcode `shiftR` 3)
            _    -> stubResult machine
        q40 = if opcode == 0x76
            then instHalt machine
            else instLD_R_R machine (shiftR opcode 3 .&. 7) (opcode .&. 7)
        q80 = stubResult machine
        qC0 = case opcode .&. 0x0f of
                0x0D -> instCALL_EXX_Prefix machine opcode
                _    -> stubResult machine


instX0 :: Machine -> Byte -> StepResult
instX0 machine opcode 
    = case opcode of
           0x00 -> return (machine, 1, 4)  -- NOP
           0x08 -> return 
                (machine         -- EX AF,AF'
                    {mCPU = cpu
                        { cpuA  = cpuA' cpu
                        , cpuA' = cpuA  cpu
                        , cpuF  = cpuF' cpu
                        , cpuF' = cpuF  cpu
                        }
                    }, 1, 4)
           0x10 -> do            -- DJNZ
                let b = cpuB cpu
                    m = machine{mCPU = cpu{cpuB = b-1}}
                if b==1 then return (m, 2, 8)
                        else do
                            o <- readMemory m (pc+1)
                            return (m {mCPU = cpu{cpuPC = plusOffset pc o}}, 2, 13)
           0x18 -> instJR 
           0x20 -> if isFlagZ cpu -- JR NZ,d   
                      then return (machine, 2, 7)
                      else instJR
           0x28 -> if isFlagZ cpu -- JR Z,d
                      then instJR
                      else return (machine, 2, 7)
           0x30 -> if isFlagC cpu -- JR NC,d   
                      then return (machine, 2, 7)
                      else instJR
           0x38 -> if isFlagC cpu -- JR C,d
                      then instJR
                      else return (machine, 2, 7)
           ____ -> error "Unknown 0x00 instruction"
    where 
        cpu = mCPU machine
        pc = cpuPC cpu
        instJR = do
            o <- readMemory machine (pc+1)
            return (machine {mCPU = cpu{cpuPC = plusOffset pc o}}, 2, 12)



---------------------------
-- LD RR,nn instructions --
---------------------------
instLD_RR_W :: Machine -> Byte -> StepResult
instLD_RR_W machine opcode = do
    let cpu = mCPU machine
    w <- readMemoryW machine (cpuPC cpu + 1)
    let cpu' = case opcode of
                    0x01 -> setBC cpu w
                    0x11 -> setDE cpu w
                    0x21 -> setHL_IX_IY cpu w
                    0x31 -> setSP cpu w
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


---------------------------------------------------------
-- LD a,(BC\DE\mem),a and LD HL\IX\IY,(mem) instructions --
---------------------------------------------------------
instLD_R_mem :: Machine -> Byte -> StepResult
instLD_R_mem machine opcode = do
    let cpu = mCPU machine
    if opcode == 0x2A
        then do
            addr <- readMemoryW machine $ cpuPC cpu + 1
            w <- readMemoryW machine addr
            let cpu' = setHL_IX_IY cpu w
            return (machine{mCPU = cpu'}, 3, 16)
        else do
            (addr, b, t) <- case opcode of
                0x0A -> return (getBC cpu, 1, 7) 
                0x1A -> return (getDE cpu, 1, 7)
                0x3A -> do 
                    addr <- readMemoryW machine $ cpuPC cpu + 1
                    return (addr, 3, 13)
                _   -> error "Unknown register in LD (mem),nn"
            a <- readMemory machine addr 
            let cpu' = cpu{cpuA = a}
            return (machine{mCPU = cpu'}, b, t)


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
        (o, b, t) <- offsetHL_IX_IY machine
        r <- readMemory machine o
        let (r', upd) = if dec 
                then (r - 1, updFlagsDec)
                else (r + 1, updFlagsInc)
        writeMemory machine o r'
        return (machine{mCPU = upd cpu r r'}, 1 + b, 7 + t)
    else do
        let r = getReg8 cpu op reg
        let (r', upd) = if dec 
                then (r - 1, updFlagsDec)
                else (r + 1, updFlagsInc)
            cpu' = upd (setReg8 cpu op reg r') r r'
        return (machine{mCPU = cpu'}, 1, 4)


-------------------------
-- LD r,n instructions --
-------------------------
instLD_R_n :: Machine -> Byte -> StepResult
instLD_R_n machine reg
    | reg == 6 = do
        (o, b, t) <- offsetHL_IX_IY machine
        n <- readMemory machine (cpuPC cpu + b + 1)
        writeMemory machine o n
        return (machine, 2 + b, 10 + t)
    | otherwise = do
        n <- readMemory machine (cpuPC cpu + 1)
        return (machine{mCPU = setReg8 cpu op reg n}, 2, 10)
    where
        cpu = mCPU machine
        op = cpuOP cpu

---------------
-- ADD HL,RR --
---------------
instADD_HL_RR :: Machine -> Byte -> StepResult
instADD_HL_RR machine opcode = return (machine{mCPU = cpu'}, 1, 11)
    where 
        cpu = mCPU machine
        r1 = getHL_IX_IY cpu
        r2 = case opcode of
                0x09 -> getBC cpu
                0x19 -> getDE cpu
                0x29 -> getHL_IX_IY cpu
                0x39 -> getSP cpu
                ____ -> error "Unknown ADD HL,RR opcode" 
        rr = r1 + r2
        cpu' = updFlagsAddW (setHL_IX_IY cpu rr) r1 rr 


-------------------------
-- LD r,r instructions --
-------------------------
instLD_R_R :: Machine -> Byte -> Byte -> StepResult
instLD_R_R machine r1 r2
    | r1 == 6 = do   -- ld (hl),r
        (addr, b, t) <- offsetHL_IX_IY machine 
        writeMemory machine addr $ getReg8 cpu NoPrefix r2
        return (machine, 1+b, 7+t)
    | r2 == 6 = do          -- ld r,(hl)
        (addr, b, t) <- offsetHL_IX_IY machine
        cpu' <- setReg8 cpu NoPrefix r1 <$> readMemory machine addr
        return (machine{mCPU = cpu'}, 1+b, 7+t)
    | otherwise = return (machine{mCPU = setReg8 cpu op r1 $ getReg8 cpu op r2}, 1, 4)
    where
        cpu = mCPU machine
        op = cpuOP cpu

offsetHL_IX_IY :: Machine ->IO (Address, Address, Int)
offsetHL_IX_IY machine = case op of
    DDPrefix -> do
        o <- readMemory machine (cpuPC cpu + 1)
        return (getIX_Off cpu o, 1, 12)
    FDPrefix -> do
        o <- readMemory machine (cpuPC cpu + 1)
        return (getIY_Off cpu o, 1, 12)
    _OtherPrefix ->
        return (getHL cpu, 0, 0)
    where
        cpu = mCPU machine
        op = cpuOP cpu

instHalt :: Machine -> StepResult
instHalt machine = error "Halt - not implemented yet"
                   --return (machine, 0, 4)


instCALL_EXX_Prefix :: Machine -> Byte -> StepResult
instCALL_EXX_Prefix machine opcode = case opcode of
    0xCD -> stubResult machine
    0xDD -> return (machine{mCPU = cpu{cpuOP2 = DDPrefix}}, 1, 4)
    0xED -> return (machine{mCPU = cpu{cpuD = cpuH cpu, cpuE = cpuL cpu, cpuH = cpuD cpu, cpuL = cpuE cpu}}, 1, 4)
    0xFD -> return (machine{mCPU = cpu{cpuOP2 = FDPrefix}}, 1, 4)
    _Other -> error "Invalid 11xx1101 opcode"
    where
        cpu = mCPU machine



cbInstructions :: Machine -> Byte -> StepResult
cbInstructions machine opcode = do
    stubResult machine

edInstructions :: Machine -> Byte -> StepResult
edInstructions machine opcode = do
    stubResult machine


