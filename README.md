# MIPS Emulator

A simple MIPS processor emulator written in Haskell.

Supports MIPS instructions, register and memory operations,
binary program loading, and exit syscall (10).

Planning to add support for additional syscalls.

## Development

```sh
# Enter the development shell
nix develop

# Build the project
cabal build
```

## Usage

```haskell
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector as V
import MIPS

-- 1. Load 42 into $t0
-- 2. Load 58 into $t1
-- 3. Add $t0 and $t1, storing result in $t2
-- 4. Exit the program
exampleProgram :: ByteString
exampleProgram = runPut $ do
  -- li $t0, 42 (using lui and ori)
  putWord32be 0x3c080000 -- lui $t0, 0
  putWord32be 0x3508002a -- ori $t0, $t0, 42

  -- li $t1, 58 (using lui and ori)
  putWord32be 0x3c090000 -- lui $t1, 0
  putWord32be 0x3529003a -- ori $t1, $t1, 58

  -- add $t2, $t0, $t1
  putWord32be 0x01095020 -- add $t2, $t0, $t1

  -- Exit syscall
  putWord32be 0x34020000 -- li $v0, 0 (set exit code to 0)
  putWord32be 0x3402000a -- li $v0, 10 (syscall number 10 is exit)
  putWord32be 0x0000000c -- syscall

main :: IO ()
main = case executeProgram exampleProgram of
  Left err -> putStrLn $ "Error: " ++ err
  Right finalState -> do
    -- Check if the calculation was correct (42 + 58 = 100)
    let t2Value = getRegister T2 finalState
    if t2Value == 100
      then putStrLn "Program executed successfully! Result is 100."
      else putStrLn $ "Program executed but result is incorrect: " ++ show t2Value
```
