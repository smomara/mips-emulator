module MIPS (
  VM,
  MipsState,
  Register (..),
  runVM,
  initialState,
  getRegister,
  step,
  run,
  loadProgram,
  loadProgramFromFile,
  executeProgram,
  executeProgramFromFile,
) where

import Control.Monad.State (get)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import MIPS.Decoder (decodeInstruction)
import MIPS.Instructions (executeInstruction)
import MIPS.Loader (loadProgram)
import MIPS.Memory (readWord)
import MIPS.Registers (getPC)
import MIPS.Types (Address, ExecutionStatus (..), MipsState (status), Register (..), VM, getRegister, initialState, runVM)

step :: VM ()
step = do
  st <- get
  case status st of
    Running -> getPC >>= readWord >>= decodeInstruction >>= executeInstruction
    Exited _ -> return ()

run :: VM ()
run = do
  st <- get
  case status st of
    Running -> step >> run
    Exited _ -> return ()

loadProgramFromFile :: FilePath -> IO (VM Address)
loadProgramFromFile path = loadProgram <$> BL.readFile path

executeProgram :: ByteString -> IO (Either String MipsState)
executeProgram program = do
  (result, state) <- runVM (loadProgram program >> run) initialState
  return $ case result of
    Left str -> Left str
    Right _ -> Right state

executeProgramFromFile :: FilePath -> IO (Either String MipsState)
executeProgramFromFile path = BL.readFile path >>= executeProgram
