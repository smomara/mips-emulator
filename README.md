# MIPS Emulator

A simple MIPS processor emulator written in Haskell.

Supports MIPS instructions, register and memory operations,
binary program loading, exit syscalls (10 and 17), and print_int syscall (1).

Planning to add support for additional syscalls.

## Development

```sh
# Enter the development shell
nix develop

# Build the project
cabal build
```

## Example

```haskell
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import MIPS

-- 1. Load 42 into $t0
-- 2. Load 58 into $t1
-- 3. Add $t0 and $t1, storing result in $t2
-- 4. Print the result of the addition (stored in $t2) by loading it into $a0
-- 5. Exit the program
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

  -- Load the result of the addition ($t2) into $a0 for printing
  putWord32be 0x01402020 -- add $a0, $t2, $zero

  -- Load the syscall number 1 (print_int) into $v0
  putWord32be 0x3c020000 -- lui $v0, 0
  putWord32be 0x34420001 -- ori $v0, $v0, 1

  -- Perform the syscall to print the integer
  putWord32be 0x0000000c -- syscall

  -- Load the syscall number 10 (exit) into $v0
  putWord32be 0x3c020000 -- lui $v0, 0
  putWord32be 0x3442000a -- ori $v0, $v0, 10

  -- Perform the syscall to exit
  putWord32be 0x0000000c -- syscall

main :: IO ()
main = do
  result <- executeProgram exampleProgram
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right finalState -> do
      -- Check if the calculation was correct (42 + 58 = 100)
      let t2Value = getRegister T2 finalState
      if t2Value == 100
        then putStrLn "Program executed successfully! Result is 100."
        else putStrLn $ "Program executed but result is incorrect: " ++ show t2Value
```
