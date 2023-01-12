module LibRISCV.CmdLine where

import Data.Word
import Options.Applicative

-- BasicArgs can be combined with additional parsers using
-- the <*> applicative operator provided by optparse-applicative.
--
-- See: https://github.com/pcapriotti/optparse-applicative#applicative

data BasicArgs = BasicArgs
    { memAddr  :: Word32
    , memStart :: Word32
    , trace    :: Bool
    , putRegs  :: Bool
    , file     :: String }

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
