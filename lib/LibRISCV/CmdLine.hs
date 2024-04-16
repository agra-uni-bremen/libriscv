-- | Implementation of common command-line arguments using <https://hackage.haskell.org/package/optparse-applicative  optparse-applicative>.
module LibRISCV.CmdLine where

import Data.Word
import Options.Applicative

-- | t'BasicArgs' can be combined/extended with additional parsers using
-- the '<*>' applicative operator provided by "Options.Applicative".
data BasicArgs = BasicArgs
    { memAddr  :: Word32
    -- ^ Start address of the general-purpose memory.
    , memStart :: Word32
    -- ^ Size of the memory in bytes.
    , trace    :: Bool
    -- ^ Whether to enable instruction tracing.
    , putRegs  :: Bool
    -- ^ Whether to print all register values at the end.
    , file     :: String
    -- ^ Path to ELf file which should be executed.
    }
-- TODO: Rename memStart to memSize.

-- | "Options.Applicative" parser for t'BasicArgs'.
basicArgs :: Parser BasicArgs
basicArgs = BasicArgs
    <$> option auto
        ( long "memory-start"
       <> short 'm'
       <> value 0x10000 )
    <*> option auto
        ( long "memory-size"
       <> short 's'
       <> value (1024 * 1024 * 1) -- 1 MB RAM
       <> help "Size of the memory region" )
    <*> switch
        ( long "trace"
       <> short 't'
       <> help "Whether to trace all executed instructions" )
    <*> switch
        ( long "registers"
       <> short 'r'
       <> help "Whether to print all register values" )
    <*> argument str (metavar "FILE")
