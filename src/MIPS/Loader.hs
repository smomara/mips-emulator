module MIPS.Loader where

import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word32)
import MIPS.Memory
import MIPS.Registers
import MIPS.Types

defaultEntryPoint :: Address
defaultEntryPoint = 0x00400000

defaultStackPointer :: Word32
defaultStackPointer = 0x7FFFFFFC

loadExecutable :: ByteString -> VM Address
loadExecutable bs = do
  setPC defaultEntryPoint
  writeRegister SP defaultStackPointer
  loadProgramBytes defaultEntryPoint bs

loadProgramBytes :: Address -> ByteString -> VM Address
loadProgramBytes addr bs
  | BL.null bs = return addr
  | otherwise = do
      let (chunk, rest) = BL.splitAt 4 bs
      if BL.length chunk == 4
        then do
          let word = runGet getWord32be chunk
          writeWord addr word
          loadProgramBytes (addr + 4) rest
        else do
          mapM_
            (\(i, b) -> writeByte (addr + fromIntegral i :: Word32) b)
            (zip [0 :: Word32 ..] (BL.unpack chunk)) -- TODO overflow?
          return $ addr + fromIntegral (BL.length chunk)

loadExecutableFromFile :: FilePath -> IO (Either String Address)
loadExecutableFromFile path = do
  contents <- BL.readFile path
  fst <$> runVM (loadExecutable contents) initialState
