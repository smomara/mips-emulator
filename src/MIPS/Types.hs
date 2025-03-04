module MIPS.Types where

import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32, Word8)

type Address = Word32

data Register
  = Zero -- the constant value 0
  | AT -- assembler temporary
  | V0 -- procedure return values
  | V1
  | A0 -- procedure arguments
  | A1
  | A2
  | A3
  | T0 -- temporary variables
  | T1
  | T2
  | T3
  | T4
  | T5
  | T6
  | T7
  | S0 -- saved variables
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | T8 -- more temporary variables
  | T9
  | K0 -- operating system temporaries
  | K1
  | GP -- global pointer
  | SP -- stack pointer
  | FP -- frame pointer
  | RA -- procedure return address
  deriving (Show, Eq, Ord, Enum)

type RegisterFile = Vector Word32

data SpecialRegisters = SpecialRegisters
  { pc :: Address
  , hi :: Word32
  , lo :: Word32
  }
  deriving (Show, Eq)

type Memory = Map Address Word8

data MipsState = MipsState
  { memory :: Memory
  , registers :: RegisterFile
  , specialRegisters :: SpecialRegisters
  }
  deriving (Show, Eq)

type VM a = StateT MipsState (Either String) a

throwError :: String -> VM a
throwError = lift . Left

initialRegisters :: RegisterFile
initialRegisters = V.replicate 32 0

initialState :: MipsState
initialState =
  MipsState
    { memory = M.empty
    , registers = initialRegisters
    , specialRegisters = SpecialRegisters 0 0 0
    }

runVM :: VM a -> MipsState -> Either String (a, MipsState)
runVM = runStateT
