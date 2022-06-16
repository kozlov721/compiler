{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Utils ( sizedReg
             , sizedInst
             ) where

import Data.Map ( Map )

import qualified Data.Map as M

regs :: Map String (Map Int String)
regs = M.fromList
    $ [ ("rax", [ (32, "eax")
                , (16, "ax")
                , (8, "al")])
      , ("rbx", [ (32, "ebx")
                , (16, "bx")
                , (8, "bl")])
      , ("rcx", [ (32, "ecx")
                , (16, "cx")
                , (8, "cl")])
      , ("rdx", [ (32, "edx")
                , (16, "dx")
                , (8, "dl")])
      , ("rsi", [ (32, "esi")
                , (16, "si")
                , (8, "sil")])
      , ("rdi", [ (32, "edi")
                , (16, "di")
                , (8, "dil")])
      , ("rsp", [ (32, "esp")
                , (16, "sp")
                , (8, "spl")])
      , ("rbp", [ (32, "ebp")
                , (16, "bp")
                , (8, "bpl")])
      ] ++
      [ let reg = "r" ++ show n in
        (reg, [ (32, reg ++ "d")
              , (16, reg ++ "w")
              , (8, reg ++ "b")
              ])
      | n <- [8..15]
      ]


sizedReg :: String -> Int -> String
sizedReg reg 64 = reg
sizedReg reg size = regs M.! reg M.! size

sizedInst :: String -> Int -> String
sizedInst inst 8 = inst ++ "b"
sizedInst inst 16 = inst ++ "w"
sizedInst inst 32 = inst ++ "l"
sizedInst inst 64 = inst ++ "q"
