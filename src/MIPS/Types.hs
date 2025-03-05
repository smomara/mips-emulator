module MIPS.Types where

import Control.Monad.State
import Data.Int (Int16)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word8)

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

data Instruction
  = -- R-type instructions
    Add Register Register Register -- add $d,$s,$t
  | Addu Register Register Register -- addu $d,$s,$t
  | Sub Register Register Register -- sub $d,$s,$t
  | Subu Register Register Register -- subu $d,$s,$t
  | And Register Register Register -- and $d,$s,$t
  | Or Register Register Register -- or $d,$s,$t
  | Slt Register Register Register -- slt $d,$s,$t
  | Mult Register Register -- mult $s,$t
  | Div Register Register -- div $s,$t
  | Sll Register Register Int -- sll $d,$t,shamt
  | Srl Register Register Int -- srl $d,$t,shamt
  | Jr Register -- jr $s
  | Mfhi Register -- mfhi $d
  | Mflo Register -- mflo $d
  -- I-type instructions (Immediate)
  | Addi Register Register Int16 -- addi $t,$s,imm
  | Addiu Register Register Int16 -- addiu $t,$s,imm
  | Andi Register Register Word16 -- andi $t,$s,imm
  | Ori Register Register Word16 -- ori $t,$s,imm
  | Slti Register Register Int16 -- slti $t,$s,imm
  | Lui Register Word16 -- lui $t,imm
  | Lw Register Int16 Register -- lw $t,offset($s)
  | Sw Register Int16 Register -- sw $t,offset($s)
  | Beq Register Register Int16 -- beq $s,$t,offset
  | Bne Register Register Int16 -- bne $s,$t,offset
  -- J-type instructions (Jump)
  | J Address -- j target
  | Jal Address -- jal target
  -- System calls
  | Syscall -- syscall
  deriving (Show, Eq)

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
