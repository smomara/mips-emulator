module MIPS (
  module MIPS.Types,
  module MIPS.Memory,
  module MIPS.Registers,
  module MIPS.Instructions,
  module MIPS.Decoder,
  step,
  run,
) where

import MIPS.Decoder
import MIPS.Instructions
import MIPS.Memory
import MIPS.Registers
import MIPS.Types

step :: VM ()
step = getPC >>= readWord >>= decodeInstruction >>= executeInstruction

run :: Int -> VM ()
run n
  | n <= 0 = return ()
  | otherwise = step >> run (n - 1)
