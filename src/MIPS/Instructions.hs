module MIPS.Instructions where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import MIPS.Memory (readWord, writeWord)
import MIPS.Registers
import MIPS.Types

signExtend16 :: Int16 -> Int32
signExtend16 = fromIntegral

zeroExtend16 :: Word16 -> Word32
zeroExtend16 = fromIntegral

executeInstruction :: Instruction -> VM ()
executeInstruction instr = case instr of
  -- R-type instructions
  Add rd rs rt -> do
    val1 <- readRegister rs
    val2 <- readRegister rt
    writeRegister rd $ val1 + val2
    incrementPC
  Addu rd rs rt -> do
    val1 <- readRegister rs
    val2 <- readRegister rt
    writeRegister rd $ val1 + val2
    incrementPC
  Sub rd rs rt -> do
    val1 <- readRegister rs
    val2 <- readRegister rt
    writeRegister rd $ val1 - val2
    incrementPC
  Subu rd rs rt -> do
    val1 <- readRegister rs
    val2 <- readRegister rt
    writeRegister rd $ val1 - val2
  And rd rs rt -> do
    val1 <- readRegister rs
    val2 <- readRegister rt
    writeRegister rd $ val1 .&. val2
    incrementPC
  Or rd rs rt -> do
    val1 <- readRegister rs
    val2 <- readRegister rt
    writeRegister rd $ val1 .|. val2
    incrementPC
  Slt rd rs rt -> do
    val1 <- readRegister rs
    val2 <- readRegister rt
    let result =
          if (fromIntegral val1 :: Int32) < (fromIntegral val2 :: Int32)
            then 1
            else 0
    writeRegister rd result
    incrementPC
  Mult rs rt -> do
    val1 <- readRegister rs
    val2 <- readRegister rt
    let prod = fromIntegral val1 * fromIntegral val2 :: Int64
    setHI $ fromIntegral (prod `shiftR` 32)
    setLO $ fromIntegral prod
    incrementPC
  Div rs rt -> do
    val1 <- readRegister rs
    val2 <- readRegister rt
    if val2 == 0
      then throwError "Division by zero"
      else do
        let sval1 = fromIntegral val1 :: Int32
            sval2 = fromIntegral val2 :: Int32
        setLO $ fromIntegral (sval1 `div` sval2)
        setHI $ fromIntegral (sval1 `mod` sval2)
        incrementPC
  Sll rd rt shamt -> do
    val <- readRegister rt
    writeRegister rd $ val `shiftL` shamt
    incrementPC
  Srl rd rt shamt -> do
    val <- readRegister rt
    writeRegister rd $ val `shiftR` shamt
    incrementPC
  Jr rs -> do
    target <- readRegister rs
    setPC target
  Mfhi rd -> do
    val <- getHI
    writeRegister rd val
    incrementPC
  Mflo rd -> do
    val <- getLO
    writeRegister rd val
    incrementPC
  -- I-type instructions
  Addi rt rs imm -> do
    val <- readRegister rs
    let extended = signExtend16 imm
    writeRegister rt $ val + fromIntegral extended
    incrementPC
  Addiu rt rs imm -> do
    val <- readRegister rs
    let extended = fromIntegral imm :: Word32
    writeRegister rt $ val + extended
    incrementPC
  Andi rt rs imm -> do
    val <- readRegister rs
    let extended = zeroExtend16 imm
    writeRegister rt $ val .&. extended
    incrementPC
  Ori rt rs imm -> do
    val <- readRegister rs
    let extended = zeroExtend16 imm
    writeRegister rt $ val .|. extended
    incrementPC
  Slti rt rs imm -> do
    val <- readRegister rs
    let sval = fromIntegral val :: Int32
    let extended = signExtend16 imm
    let result = if sval < extended then 1 else 0
    writeRegister rt result
    incrementPC
  Lui rt imm -> do
    let shifted = zeroExtend16 imm `shiftL` 16
    writeRegister rt shifted
    incrementPC
  Lw rt offset rs -> do
    baseAddr <- readRegister rs
    let addr = baseAddr + fromIntegral (signExtend16 offset)
    val <- readWord addr
    writeRegister rt val
    incrementPC
  Sw rt offset rs -> do
    baseAddr <- readRegister rs
    let addr = baseAddr + fromIntegral (signExtend16 offset)
    val <- readRegister rt
    writeWord addr val
    incrementPC
  Beq rs rt offset -> do
    val1 <- readRegister rs
    val2 <- readRegister rt
    currPC <- getPC
    if val1 == val2
      then setPC $ currPC + 4 + fromIntegral (signExtend16 offset) * 4
      else incrementPC
  Bne rs rt offset -> do
    val1 <- readRegister rs
    val2 <- readRegister rt
    currPC <- getPC
    if val1 /= val2
      then setPC $ currPC + 4 + fromIntegral (signExtend16 offset) * 4
      else incrementPC
  J target -> do
    setPC (target * 4)
  Jal target -> do
    currPC <- getPC
    writeRegister RA $ currPC + 4
    setPC $ target * 4
  Syscall -> do
    syscallNum <- readRegister V0
    case syscallNum of
      -- print_int
      1 -> do
        int <- readRegister A0
        liftIO $ print int
        incrementPC
      -- exit
      10 -> do
        exitCode <- readRegister A0
        st <- get
        put $ st{status = Exited (fromIntegral exitCode)}
      _ -> throwError $ "Unsupported syscall: " ++ show syscallNum
