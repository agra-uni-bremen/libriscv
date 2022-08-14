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
import Register
import Decoder
import Memory

-- Architectural state of the executor.
type ArchState = (RegisterFile, Memory)

-- Create a new architectural state with the given memory.
mkArchState :: Memory -> IO (ArchState)
mkArchState mem = do
    r <- mkRegFile
    return (r, mem)

-- Execute a given instruction, with fetch & decode done separately.
execute' :: ArchState -> Address -> Instruction -> IO ()
execute' s@(r, m) _ (Add rd rs1 rs2) = do
    r1 <- readRegister r rs1
    r2 <- readRegister r rs2
    writeRegister r rd $ r1 + r2
execute' s@(r, m) _ (Addi imm rd rs1) = do
    r1 <- readRegister r rs1
    writeRegister r rd $ (fromIntegral $ (fromIntegral r1) + imm)
execute' s@(r, m) _ (Lw imm rd rs1) = do
    r1 <- readRegister r rs1
    -- TODO: Alignment handling
    let addr = (fromIntegral r1 :: Int32) + (fromIntegral imm) in
        do
            word <- loadWord m $ fromIntegral addr
            writeRegister r rd word
execute' s@(r, m) pc (Auipc rd imm) = do
    writeRegister r rd $ fromIntegral $ (fromIntegral pc) + imm
execute' _ _ InvalidInstruction = pure () -- XXX: ignore for now
execute' _ _ _ = error "not implemented"

-- Fetch, decode and execute the instruction at the given address.
-- Returns the executed instruction and the address of the next instruction.
execute :: Tracer t => ArchState -> Maybe t -> Address -> IO (Instruction, Address)
execute state@(r, m) tracer addr = do
    word <- loadWord m addr
    inst <- pure $ decode word

    case tracer of
        (Just t) -> trace t addr inst
        Nothing  -> pure ()
    execute' state addr inst

    -- Address of the next instruction
    return $ (inst, addr + 4)

-- Execute til the first invalid instruction.
executeAll :: Tracer t => ArchState -> Maybe t -> Address -> IO ()
executeAll state tracer addr = do
    (instr, nextAddr) <- execute state tracer addr
    case instr of
        InvalidInstruction -> pure ()
        _ -> executeAll state tracer nextAddr
