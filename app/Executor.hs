module Executor where

import Data.Word
import Register
import Decoder
import Memory

-- Architectural state of the executor.
type ArchState = (RegisterFile, Memory)

-- Create a new architectural state with a memory of the given size.
mkArchState :: Word32 -> IO (ArchState)
mkArchState size = do
    r <- mkRegFile
    m <- mkMemory size
    return (r, m)

-- Execute a given instruction, with fetch & decode done separately.
execute' :: ArchState -> Instruction -> IO ()
execute' s@(r, m) (Add rd rs1 rs2) = do
    r1 <- readRegister r rs1
    r2 <- readRegister r rs2
    writeRegister r rd $ r1 + r2
execute' (r, m) _ = error "not implemented"

-- Fetch, decode and execute the instruction at the given address.
-- Returns the executed instruction and the address of the next instruction.
execute :: ArchState -> Address -> IO (Instruction, Address)
execute state@(r, m) addr = do
    word <- loadWord m addr
    inst <- pure $ decode word

    -- Address of the next instruction
    return $ (inst, addr + 4)

-- Execute til the first invalid instruction.
executeAll :: ArchState -> Address -> IO ()
executeAll state addr = do
    (instr, nextAddr) <- execute state addr
    case instr of
        InvalidInstruction -> pure ()
        _ -> executeAll state nextAddr
