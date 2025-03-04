module MIPS.Memory where

import Control.Monad (when)
import Control.Monad.State
import Data.Bits
import qualified Data.Map.Strict as M
import Data.Word (Word32, Word8)
import MIPS.Types

readByte :: Address -> VM Word8
readByte addr = do
  mem <- gets memory
  return $ M.findWithDefault 0 addr mem

writeByte :: Address -> Word8 -> VM ()
writeByte addr val = do
  st <- get
  let mem = memory st
  put $ st{memory = M.insert addr val mem}

readWord :: Address -> VM Word32
readWord addr = do
  when (addr `mod` 4 /= 0) $
    throwError $
      "Unaligned memory access at " ++ show addr

  b0 <- readByte addr
  b1 <- readByte (addr + 1)
  b2 <- readByte (addr + 2)
  b3 <- readByte (addr + 3)

  return $
    fromIntegral b0
      .|. (fromIntegral b1 `shiftL` 8)
      .|. (fromIntegral b2 `shiftL` 16)
      .|. (fromIntegral b3 `shiftL` 24)

writeWord :: Address -> Word32 -> VM ()
writeWord addr val = do
  when (addr `mod` 4 /= 0) $
    throwError $
      "Unaligned memory access at " ++ show addr

  let b0 = fromIntegral $ val .&. 0xFF
      b1 = fromIntegral $ (val `shiftR` 8) .&. 0xFF
      b2 = fromIntegral $ (val `shiftR` 16) .&. 0xFF
      b3 = fromIntegral $ (val `shiftR` 24) .&. 0xFF

  writeByte addr b0
  writeByte (addr + 1) b1
  writeByte (addr + 2) b2
  writeByte (addr + 3) b3
