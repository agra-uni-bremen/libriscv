module Executor
(
    ArchState
    , mkArchState
    , executeAll
)
where

import Tracer ( Tracer(..) )
import Data.Word ()
import Data.Int ()
import Data.Bits ( Bits((.&.)) )
import Register
    ( mkRegFile,
      readPC,
      readRegister,
      writePC,
      writeRegister,
      RegisterFile )
import Decoder
import Memory ( loadWord, storeWord, Address, Memory )
import Control.Monad (when)

-- Architectural state of the executor.
type ArchState = (RegisterFile, Memory)

-- Create a new architectural state with the given memory and the given
-- initial program counter value.
mkArchState :: Memory -> Address -> IO ArchState
mkArchState mem entry = do
    r <- mkRegFile
    writePC r entry
    pure (r, mem)

-- Execute a given instruction, with fetch & decode done separately.
-- Receives architectural state, address of the instruction and the instruction itself.
execute' :: ArchState -> Address -> Instruction -> IO ()
execute' s@(r, m) _ (Add rd rs1 rs2) = do
    r1 <- readRegister r rs1
    r2 <- readRegister r rs2
    writeRegister r rd $ r1 + r2
execute' s@(r, m) _ (Addi imm rd rs1) = do
    r1 <- readRegister r rs1
    writeRegister r rd $ r1 + getImmediate imm
execute' s@(r, m) _ (Lw imm rd rs1) = do
    r1 <- readRegister r rs1
    -- TODO: Alignment handling
    word <- loadWord m $ fromIntegral (r1 + getImmediate imm)
    writeRegister r rd $ fromIntegral word
execute' s@(r, m) _ (Sw imm rs1 rs2) = do
    r1 <- readRegister r rs1
    r2 <- readRegister r rs2
    -- TODO: Alignment handling
    storeWord m (fromIntegral $ r1 + getImmediate imm) $ fromIntegral r2
execute' s@(r, m) pc (Blt imm rs1 rs2) = do
    r1 <- readRegister r rs1
    r2 <- readRegister r rs2

    -- TODO: Alignment handling
    when (r1 < r2) $
        writePC r $ fromIntegral $ fromIntegral pc + getImmediate imm
execute' s@(r, m) pc (Jal imm rd) = do
    nextInstr <- readPC r
    -- TODO: Alignment handling
    writePC r $ fromIntegral $ fromIntegral pc + getImmediate imm
    writeRegister r rd $ fromIntegral nextInstr
execute' s@(r, m) pc (Jalr imm rs1 rd) = do
    nextInstr <- readPC r
    rs1Val <- readRegister r rs1 
    writePC r $ fromIntegral $ (rs1Val + getImmediate imm) .&. 0xfffffffe
    writeRegister r rd $ fromIntegral nextInstr
execute' s@(r, m) _ (Lui rd imm) = 
    writeRegister r rd $ getImmediate imm
execute' s@(r, m) pc (Auipc rd imm) =
    writeRegister r rd $ fromIntegral pc + getImmediate imm
execute' _ _ InvalidInstruction = pure () -- XXX: ignore for now
execute' _ _ _ = error "not implemented"

-- Fetch, decode and execute the instruction at current pc.
-- Returns the executed instruction.
execute :: Tracer t => ArchState -> Maybe t -> IO Instruction
execute state@(r, m) tracer = do
    pc   <- readPC r
    word <- loadWord m pc
    let inst = decode word
    case tracer of
        Just t -> trace t pc inst
        Nothing  -> pure ()

    -- Increment PC before execute', allows setting PC to to
    -- different values in execute' for jumps and branches.
    writePC r $ pc + 4
    execute' state pc inst

    return inst

-- Execute til the first invalid instruction.
executeAll :: Tracer t => ArchState -> Maybe t -> IO ()
executeAll state tracer = do
    instr <- execute state tracer
    case instr of
        InvalidInstruction -> pure ()
        _ -> executeAll state tracer
