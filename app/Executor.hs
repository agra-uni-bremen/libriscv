module Executor
(
    ArchState
    , mkArchState
    , executeAll
)
where

import Tracer
import Data.Word
import Data.Int
import Data.Bits
import Register
import Decoder
import Memory

-- Architectural state of the executor.
type ArchState = (RegisterFile, Memory)

-- Create a new architectural state with the given memory and the given
-- initial program counter value.
mkArchState :: Memory -> Address -> IO (ArchState)
mkArchState mem entry = do
    r <- mkRegFile
    writePC r entry
    return (r, mem)

-- Execute a given instruction, with fetch & decode done separately.
execute' :: ArchState -> Instruction -> IO ()
execute' s@(r, m) (Add rd rs1 rs2) = do
    r1 <- readRegister r rs1
    r2 <- readRegister r rs2
    writeRegister r rd $ r1 + r2
execute' s@(r, m) (Addi imm rd rs1) = do
    r1 <- readRegister r rs1
    writeRegister r rd $ r1 + imm
execute' s@(r, m) (Lw imm rd rs1) = do
    r1 <- readRegister r rs1
    -- TODO: Alignment handling
    word <- loadWord m $ fromIntegral (r1 + imm)
    writeRegister r rd $ fromIntegral word
execute' s@(r, m) (Sw imm rs1 rs2) = do
    r1 <- readRegister r rs1
    r2 <- readRegister r rs2
    -- TODO: Alignment handling
    storeWord m (fromIntegral $ r1 + imm) $ fromIntegral r2
execute' s@(r, m) (Lui rd imm) = do
    writeRegister r rd imm
execute' s@(r, m) (Auipc rd imm) = do
    pc <- readPC r
    writeRegister r rd $ (fromIntegral pc) + imm
execute' _ InvalidInstruction = pure () -- XXX: ignore for now
execute' _ _ = error "not implemented"

-- Fetch, decode and execute the instruction at current pc.
-- Returns the executed instruction.
execute :: Tracer t => ArchState -> Maybe t -> IO (Instruction)
execute state@(r, m) tracer = do
    pc   <- readPC r
    word <- loadWord m pc
    inst <- pure $ decode word

    case tracer of
        (Just t) -> trace t pc inst
        Nothing  -> pure ()
    execute' state inst

    -- Address of the next instruction
    writePC r $ pc + 4
    return inst

-- Execute til the first invalid instruction.
executeAll :: Tracer t => ArchState -> Maybe t -> IO ()
executeAll state tracer = do
    instr <- execute state tracer
    case instr of
        InvalidInstruction -> pure ()
        _ -> executeAll state tracer
