{-# LANGUAGE OverloadedLists       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Utils where


import Syntax

import Control.Lens
import Control.Monad.State
import Control.Monad.Tardis
import Control.Monad.Writer
import Data.List.Extra      ( lower, upper )
import Data.Map             ( Map )
import Text.Format

import qualified Data.Map as M


data Register = RAX | RBX | RCX | RDX | RSI | RDI | RSP | RBP
              | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
              deriving ( Show, Read, Eq, Ord )

type Offset = Int
type Size = Int
data VarsTable = VarsTable { _table     :: Map String (Offset, Size)
                           , _maxOffset :: Int
                           }

data FwState = FwState { _vars    :: VarsTable
                       , _funs    :: Map Identifier [Type]
                       , _indent  :: Int
                       , _globals :: Map Identifier Value
                       , _nlabels :: Int
                       , _loop    :: (String, String)
                       }

data BwState = BwState { _totVarSize :: Int }

argRegs :: [Register]
argRegs = [RDI, RSI, RDX, RCX, R8, R9]

makeLenses ''FwState
makeLenses ''BwState
makeLenses ''VarsTable
makePrisms ''Type
makeLenses ''Var

class Empty a where
  empty :: a

instance Empty VarsTable where
  empty = VarsTable { _table = mempty
                    , _maxOffset = 0
                    }

instance Empty FwState where
  empty = FwState { _vars = empty
                  , _funs = mempty
                  , _globals = mempty
                  , _indent = 0
                  , _nlabels = 0
                  , _loop = ("", "")
                  }

instance Empty BwState where
  empty = BwState { _totVarSize = 0
                  }

instance (MonadFix m) => MonadState s (TardisT bw s m) where
  get = getPast
  put = sendFuture

instance (MonadFix m) => MonadFail (TardisT bw fw m) where
  fail = error

type ASM = TardisT BwState FwState (Writer String)

showReg :: Register -> String
showReg = lower . show

regs :: Map Register (Map Int String)
regs = M.fromList
    $ [ (RAX, [ (32, "eax")
              , (16, "ax")
              , (8, "al")])
      , (RBX, [ (32, "ebx")
              , (16, "bx")
              , (8, "bl")])
      , (RCX, [ (32, "ecx")
              , (16, "cx")
              , (8, "cl")])
      , (RDX, [ (32, "edx")
              , (16, "dx")
              , (8, "dl")])
      , (RSI, [ (32, "esi")
              , (16, "si")
              , (8, "sil")])
      , (RDI, [ (32, "edi")
              , (16, "di")
              , (8, "dil")])
      , (RSP, [ (32, "esp")
              , (16, "sp")
              , (8, "spl")])
      , (RBP, [ (32, "ebp")
              , (16, "bp")
              , (8, "bpl")])
      ] ++
      [ let reg = "r" ++ show n in
        (read (upper reg), [ (32, reg ++ "d")
              , (16, reg ++ "w")
              , (8, reg ++ "b")
              ])
      | n <- [8..15]
      ]

opTable :: Num a => String -> Maybe (a -> a -> a)
opTable "+" = Just (+)
opTable "-" = Just (-)
opTable "*" = Just (*)
opTable _   = Nothing

sizedReg :: Register -> Int -> String
sizedReg reg 64   = showReg reg
sizedReg reg size = regs M.! reg M.! size

sizedInst :: String -> Int -> String
sizedInst inst 8  = inst ++ "b"
sizedInst inst 16 = inst ++ "w"
sizedInst inst 32 = inst ++ "l"
sizedInst inst 64 = inst ++ "q"

infixl 4 <@>
(<@>) :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
f <@> m = mapM_ f m

showValue :: Value -> String
showValue (I x)             = show x
showValue (D x)             = show x
showValue (C x)             = show x
showValue (A str@((C _):_)) = show $ map (\(C c) -> c) str
showValue (A [])            = ""

sizeof :: Type -> Int
sizeof Char_          = 8
sizeof Short_         = 16
sizeof Int_           = 32
sizeof Long_          = 64
sizeof (Pointer_ _ _) = 64
sizeof VarArgs_       = 64

saveResult :: Identifier -> ASM ()
saveResult name = do
    use (vars . table . at name) >>= \case
        Nothing -> error $ "Assignment to an undefined variable " ++ show name
        Just (n, size)  -> fwrite "{0} %{1}, -{2}(%rbp)"
            [sizedInst "mov" size, sizedReg RAX size, show n]

withReg :: String -> ASM a -> ASM a
withReg reg m = do
    fwrite "push %{0}" [reg]
    a <- m
    fwrite "pop %{0}" [reg]
    pure a

write :: String -> ASM ()
write str = do
    n <- getsPast _indent
    lift $ tell $ str `indentBy` n ++ "\n"
  where
    indentBy str n = replicate n ' ' ++ str

label :: String -> ASM ()
label str = do
    indent -= 2
    write $ str ++ ":"
    indent += 2

newLabel :: ASM String
newLabel = do
    n <- nlabels <<+= 1
    pure $ format ".L0{0}" [show n]

fwrite :: String -> [String] -> ASM ()
fwrite str = write . format str
