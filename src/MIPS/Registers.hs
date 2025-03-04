module MIPS.Registers where

import Control.Monad.State
import qualified Data.Vector as V
import Data.Word (Word32)
import MIPS.Types

readRegister :: Register -> VM Word32
readRegister Zero = return 0
readRegister reg = do
  regs <- gets registers
  return $ regs V.! fromEnum reg

writeRegister :: Register -> Word32 -> VM ()
writeRegister Zero _ = return () -- Can't write to $zero
writeRegister reg val = do
  st <- get
  let regs = registers st
  let regs' = regs V.// [(fromEnum reg, val)]
  put $ st{registers = regs'}

getPC :: VM Address
getPC = gets (pc . specialRegisters)

setPC :: Address -> VM ()
setPC addr = do
  st <- get
  let specRegs = specialRegisters st
  put $ st{specialRegisters = specRegs{pc = addr}}

getHI :: VM Word32
getHI = gets (hi . specialRegisters)

setHI :: Word32 -> VM ()
setHI val = do
  st <- get
  let specRegs = specialRegisters st
  put $ st{specialRegisters = specRegs{hi = val}}

getLO :: VM Word32
getLO = gets (lo . specialRegisters)

setLO :: Word32 -> VM ()
setLO val = do
  st <- get
  let specRegs = specialRegisters st
  put $ st{specialRegisters = specRegs{lo = val}}

incrementPC :: VM ()
incrementPC = do
  addr <- getPC
  setPC (addr + 4)
