module MIPS (
  VM,
  MipsState,
  ExecutionStatus (..),
  runVM,
  initialState,
  step,
  run,
  loadProgram,
  loadProgramFromFile,
  execute,
  executeFile,
) where

import Control.Monad.State (get)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import MIPS.Decoder (decodeInstruction)
import MIPS.Instructions (executeInstruction)
import MIPS.Loader (loadExecutable)
import MIPS.Memory (readWord)
import MIPS.Registers (getPC)
import MIPS.Types (ExecutionStatus (..), MipsState (status), VM, initialState, runVM)

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

loadProgram :: ByteString -> Either String MipsState
loadProgram program = snd <$> runVM (loadExecutable program) initialState

loadProgramFromFile :: FilePath -> IO (Either String MipsState)
loadProgramFromFile path = loadProgram <$> BL.readFile path

execute :: ByteString -> Either String MipsState
execute program = do
  state <- loadProgram program
  snd <$> runVM run state

executeFile :: FilePath -> IO (Either String MipsState)
executeFile path = execute <$> BL.readFile path
