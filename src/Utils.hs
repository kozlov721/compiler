{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Utils where


import AST

import Control.Lens
import Control.Monad.State
import Control.Monad.Tardis
import Control.Monad.Writer
import Data.List.Extra      ( lower, upper )
import Data.Map             ( Map )
import Data.Maybe           ( fromJust )
import Text.Format

import qualified Data.Map as M


type Offset = Int
type VarRecord = (Offset, Type)
type VarsTable = Map String VarRecord

data Register = RAX | RBX | RCX | RDX | RSI | RDI | RSP | RBP
              | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15
              deriving ( Show, Read, Eq, Ord )

data Size = B | W | L | Q deriving ( Show, Eq, Ord )

data FwState = FwState { _vars    :: VarsTable
                       , _funs    :: Map Identifier [Type]
                       , _indent  :: Int
                       , _globals :: Map Identifier Value
                       , _nlabels :: Int
                       , _loop    :: (String, String)
                       }

newtype BwState = BwState { _maxOffset :: Int }

argRegs :: [Register]
argRegs = [RDI, RSI, RDX, RCX, R8, R9]

makeLenses ''FwState
makeLenses ''BwState

class Empty a where
  empty :: a

instance Empty FwState where
  empty = FwState { _vars = mempty
                  , _funs = mempty
                  , _globals = mempty
                  , _indent = 0
                  , _nlabels = 0
                  , _loop = ("", "")
                  }

instance Empty BwState where
  empty = BwState { _maxOffset = 0
                  }

instance (MonadFix m) => MonadState s (TardisT bw s m) where
  get = getPast
  put = sendFuture

instance (MonadFix m) => MonadFail (TardisT bw fw m) where
  fail = error

type ASM = TardisT BwState FwState (Writer String)


pattern Dereference e = Op "*" Nothing (Just e)
pattern Reference e   = Op "&" Nothing (Just e)
pattern Binary op l r = Op op (Just l) (Just r)
pattern Prefix op e   = Op op Nothing (Just e)
pattern Postfix op e  = Op op (Just e) Nothing


checkUndeclared :: Identifier -> ASM ()
checkUndeclared name = use (vars . at name) >>= \case
    Just _  -> fail $ "Variable " ++ show name ++ " already declared."
    Nothing -> pure ()

saveVar :: Type -> Identifier -> Offset -> ASM ()
saveVar t name o = do
    offset <- getsFuture _maxOffset
    modifyBackwards (maxOffset +~ o)
    vars . at name ?= (offset, t)

getVar :: Identifier -> ASM VarRecord
getVar name = use (vars . at name) >>= \case
    Nothing -> fail $ "usage of undeclared varialbe " ++ show name
    Just x  -> pure x

tryLiftV :: (Int -> Int -> Int) -> Value -> Value -> Maybe Value
tryLiftV f (I a) (I b) = Just $ I $ f a b
tryLiftV f (C a) (I b) = Just $ I $ f (fromEnum a) b
tryLiftV f (I a) (C b) = Just $ I $ f a (fromEnum b)
tryLiftV f (C a) (C b) = Just $ I $ f (fromEnum b) (fromEnum b)
tryLiftV _ _ _         = Nothing

simplify :: Expression -> Expression
simplify e@(Op op (Just (Constant lhs)) (Just (Constant rhs))) =
    case opTable op of
        Just f  -> maybe e Constant (tryLiftV f lhs rhs)
        Nothing -> e
simplify e@(Op op lhs rhs)
    | lhs /= lhs' || rhs /= rhs' = simplify (Op op lhs' rhs')
    | otherwise = e
  where
    lhs' = simplify <$> lhs
    rhs' = simplify <$> rhs
simplify (Assignment lhs rhs) = Assignment (simplify lhs) (simplify rhs)
simplify e = e

showReg :: Register -> String
showReg = lower . show


opTable :: Num a => String -> Maybe (a -> a -> a)
opTable "+" = Just (+)
opTable "-" = Just (-)
opTable "*" = Just (*)
opTable _   = Nothing

sizedReg :: Register -> Size -> String
sizedReg reg Q    = showReg reg
sizedReg reg size = regs M.! reg M.! size
  where
    regs = M.fromList
        $ [ (RAX, [(L, "eax"), (W, "ax"), (B, "al" )])
          , (RBX, [(L, "ebx"), (W, "bx"), (B, "bl" )])
          , (RCX, [(L, "ecx"), (W, "cx"), (B, "cl" )])
          , (RDX, [(L, "edx"), (W, "dx"), (B, "dl" )])
          , (RSI, [(L, "esi"), (W, "si"), (B, "sil")])
          , (RDI, [(L, "edi"), (W, "di"), (B, "dil")])
          , (RSP, [(L, "esp"), (W, "sp"), (B, "spl")])
          , (RBP, [(L, "ebp"), (W, "bp"), (B, "bpl")])
          ] ++
          [ let reg = "r" ++ show n in
            (read (upper reg), [ (L, reg ++ "d")
                               , (W, reg ++ "w")
                               , (B, reg ++ "b")
                               ])
          | n <- [8..15]
          ]

sizedInst :: String -> Size -> String
sizedInst inst s = inst ++ lower (show s)

infixl 4 <@>
(<@>) :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
f <@> m = mapM_ f m

showValue :: Value -> String
showValue (I x) = show x
showValue (D x) = show x
showValue (C x) = show x
showValue (S x) = show x
showValue (A x) = error "ararys not yet supported"

sizeToBytes :: Size -> Int
sizeToBytes B = 1
sizeToBytes W = 2
sizeToBytes L = 4
sizeToBytes Q = 8

sizeof :: Type -> Size
sizeof Char_        = B
sizeof Short_       = W
sizeof Int_         = L
sizeof Long_        = Q
sizeof (Pointer_ _) = Q
sizeof VarArgs_     = Q
sizeof Float_       = L
sizeof Double_      = Q
sizeof x            = error $ "so far udefined sizeof for " ++ show (show x)

findType :: Expression -> ASM (Maybe Type)
findType (Variable name) = Just . snd <$> getVar name
findType (Binary _ lhs rhs) = findType lhs >>= \case
    Just t  -> pure $ Just t
    Nothing -> findType rhs
findType _ = pure Nothing

refToRax :: Identifier -> ASM Size
refToRax name = do
    getVar name >>= \case
        (o, Pointer_ t) -> do
            fwrite "movq -{0}(%rbp), %rax" [show o]
            pure $ sizeof t
        (o, Array_ t s) -> do
            fwrite "movq -{0}(%rbp), %rax" [show o]
            pure $ sizeof t
        _ -> fail "invalid type argument of unary '*'"

saveToOffset :: Offset -> Type -> ASM ()
saveToOffset o t = do
    let size = sizeof t
    fwrite "{0} %{1} -{2}(%rbp)"
        [sizedInst "mov" size, sizedReg RAX size, show o]

saveResult :: Identifier -> ASM ()
saveResult name = do
    use (vars . at name) >>= \case
        Nothing -> error $ "Assignment to an undefined variable " ++ show name
        Just (o, t)  -> fwrite "{0} %{1}, -{2}(%rbp)"
            [sizedInst "mov" size, sizedReg RAX size, show o]
          where size = sizeof t

withReg :: Register -> ASM a -> ASM a
withReg reg m = do
    fwrite "push %{0}" [showReg reg]
    a <- m
    fwrite "pop %{0}" [showReg reg]
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
