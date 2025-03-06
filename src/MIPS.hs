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
import MIPS.Loader (loadExecutable)
import MIPS.Memory (readWord)
import MIPS.Registers (getPC)
import MIPS.Types (ExecutionStatus (..), MipsState (status), Register (..), VM, getRegister, initialState, runVM)

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

loadProgram :: ByteString -> IO (Either String MipsState)
loadProgram program = do
  (result, state) <- runVM (loadExecutable program) initialState
  return $ case result of
    Left str -> Left str
    Right _ -> Right state

loadProgramFromFile :: FilePath -> IO (Either String MipsState)
loadProgramFromFile path = BL.readFile path >>= loadProgram

executeProgram :: ByteString -> IO (Either String MipsState)
executeProgram program = do
  (result, state) <- runVM (loadExecutable program >> run) initialState
  return $ case result of
    Left str -> Left str
    Right _ -> Right state

executeProgramFromFile :: FilePath -> IO (Either String MipsState)
executeProgramFromFile path = BL.readFile path >>= executeProgram
