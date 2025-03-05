module MIPS.Decoder where

import Data.Binary.Get
import Data.Bits (shiftR, (.&.))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16)
import Data.Word (Word32, Word8)
import MIPS.Types

showHex :: Word8 -> String
showHex w = go (fromIntegral w :: Int) ""
 where
  go 0 s = if null s then "0" else s
  go n s = go (n `div` 16) (hexDigit (n `mod` 16) : s)
  hexDigit d
    | d < 10 = toEnum $ fromEnum '0' + fromIntegral d
    | otherwise = toEnum $ fromEnum 'a' + fromIntegral (d - 10)

toByteString :: Word32 -> ByteString
toByteString w =
  BL.pack
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

parseRegister :: Get (VM Register)
parseRegister = do
  regNum <- getWord8
  return . intToRegister . fromIntegral $ regNum .&. 0x1F

intToRegister :: Int -> VM Register
intToRegister n
  | n >= 0 && n < 32 = return $ toEnum n
  | otherwise = throwError $ "Invalid register number: " ++ show n

parseRType :: Get (VM Instruction)
parseRType = do
  instruction <- getWord32be
  let rs = (instruction `shiftR` 21) .&. 0x1F -- rs (5 bits)
      rt = (instruction `shiftR` 16) .&. 0x1F -- rt (5 bits)
      rd = (instruction `shiftR` 11) .&. 0x1F -- rd (5 bits)
      shamt = (instruction `shiftR` 6) .&. 0x1F -- shamt (5 bits)
      funct = instruction .&. 0x3F -- funct (6 bits)
  return $ do
    rs_reg <- intToRegister (fromIntegral rs)
    rt_reg <- intToRegister (fromIntegral rt)
    rd_reg <- intToRegister (fromIntegral rd)
    case funct of
      0x20 -> return $ Add rd_reg rs_reg rt_reg
      0x21 -> return $ Addu rd_reg rs_reg rt_reg
      0x22 -> return $ Sub rd_reg rs_reg rt_reg
      0x23 -> return $ Subu rd_reg rs_reg rt_reg
      0x24 -> return $ And rd_reg rs_reg rt_reg
      0x25 -> return $ Or rd_reg rs_reg rt_reg
      0x2A -> return $ Slt rd_reg rs_reg rt_reg
      0x00 -> return $ Sll rd_reg rt_reg (fromIntegral shamt)
      0x02 -> return $ Srl rd_reg rt_reg (fromIntegral shamt)
      0x08 -> return $ Jr rs_reg
      0x10 -> return $ Mfhi rd_reg
      0x12 -> return $ Mflo rd_reg
      0x18 -> return $ Mult rs_reg rt_reg
      0x1A -> return $ Div rs_reg rt_reg
      0x0C -> return Syscall
      _ -> throwError $ "Unknown R-type instruction function: 0x" ++ showHex (fromIntegral funct)

parseIType :: Word8 -> Get (VM Instruction)
parseIType opcode = do
  instruction <- getWord32be
  let rs = (instruction `shiftR` 21) .&. 0x1F -- rs (5 bits)
      rt = (instruction `shiftR` 16) .&. 0x1F -- rt (5 bits)
      imm16 = fromIntegral (instruction .&. 0xFFFF) :: Int16 -- imm16 (16 bits)
  return $ do
    rs_reg <- intToRegister (fromIntegral rs)
    rt_reg <- intToRegister (fromIntegral rt)
    case opcode of
      0x08 -> return $ Addi rt_reg rs_reg imm16
      0x09 -> return $ Addiu rt_reg rs_reg imm16
      0x0C -> return $ Andi rt_reg rs_reg (fromIntegral imm16)
      0x0D -> return $ Ori rt_reg rs_reg (fromIntegral imm16)
      0x0A -> return $ Slti rt_reg rs_reg imm16
      0x0F -> return $ Lui rt_reg (fromIntegral imm16)
      0x23 -> return $ Lw rt_reg imm16 rs_reg
      0x2B -> return $ Sw rt_reg imm16 rs_reg
      0x04 -> return $ Beq rs_reg rt_reg imm16
      0x05 -> return $ Bne rs_reg rt_reg imm16
      _ -> throwError $ "Unknown I-type instruction opcode: 0x" ++ showHex opcode

parseJType :: Word8 -> Get (VM Instruction)
parseJType opcode = do
  instruction <- getWord32be
  let addr = instruction .&. 0x03FFFFFF -- address (26 bits)
  return $ case opcode of
    0x02 -> return $ J addr
    0x03 -> return $ Jal addr
    _ -> throwError $ "Unknown J-type instruction opcode: 0x" ++ showHex opcode

decodeInstruction :: Word32 -> VM Instruction
decodeInstruction word = do
  let bs = toByteString word
  case runGetOrFail parseInstruction bs of
    Left (_, _, err) -> throwError $ "Instruction parsing error: " ++ err
    Right (_, _, vmInstr) -> vmInstr

parseInstruction :: Get (VM Instruction)
parseInstruction = do
  instruction <- lookAhead getWord32be
  let opcode = fromIntegral $ (instruction `shiftR` 26) .&. 0x3F
  case opcode of
    0x00 -> parseRType
    0x02 -> parseJType opcode
    0x03 -> parseJType opcode
    _ -> parseIType opcode
