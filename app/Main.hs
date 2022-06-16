{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Main where

import System.Environment
import Control.Monad ( when )
import Data.List.Extra ( unsnoc, snoc )
import Compiler
import Text.Pretty.Simple ( pPrint )
import Data.Maybe (fromJust)
import System.Process ( callProcess )


main :: IO ()
main = do
    (args, file) <- getArgs >>= \case
        [] -> fail "you need to specify an input file"
        as -> pure . fromJust . unsnoc $ as
    src <- readFile file
    let asmFile = (init . init) file ++ ".s"
    let asm = compile src
#ifdef DEBUG
    pPrint asm
#endif
    writeFile asmFile asm
    callProcess "gcc" $ args `snoc` asmFile
