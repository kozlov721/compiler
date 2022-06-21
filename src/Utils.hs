{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Utils where


import AST

import Control.Lens
import Control.Monad.Extra  ( fromMaybeM )
import Control.Monad.State
import Control.Monad.Tardis
import Control.Monad.Writer
import Data.List.Extra      ( lower, upper )
import Data.Map             ( Map )
import Data.Maybe           ( fromJust )
import Text.Format          ( format )

import qualified Data.Map as M

type Size = Integer
type Offset = Integer
type VarRecord = (Offset, Type)
type FunRecord = (Type, [Type])
type VarsTable = Map String VarRecord

data Register = RAX | RBX | RCX | RDX | RSI | RDI | RSP | RBP
              | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15
              deriving ( Show, Read, Eq, Ord )

data FwState = FwState { _vars     :: VarsTable
                       , _funs     :: Map Identifier FunRecord
                       , _structs  :: Map Identifier [Var]
                       , _unions   :: Map Identifier [Var]
                       , _typedefs :: Map Identifier Type
                       , _indent   :: Int
                       , _globals  :: Map Identifier Value
                       , _nlabels  :: Int
                       , _loop     :: (String, String)
                       }

newtype BwState = BwState { _maxOffset :: Integer }

argRegs :: [Register]
argRegs = [RDI, RSI, RDX, RCX, R8, R9]

makeLenses ''FwState
makeLenses ''BwState

class Empty a where
  empty :: a

instance Empty FwState where
  empty = FwState { _vars = mempty
                  , _funs = mempty
                  , _structs = mempty
                  , _unions = mempty
                  , _typedefs = mempty
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


pattern Dereference  e = Prefix "*" e
pattern Reference    e = Prefix "&" e
pattern Index      l r = Infix "[]" l r

infixl 4 <@>
(<@>) :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
f <@> m = mapM_ f m


checkUndeclared :: Identifier -> ASM ()
checkUndeclared name = use (vars . at name) >>= \case
    Just _  -> fail $ "Variable " ++ show name ++ " already declared."
    Nothing -> pure ()

saveVar :: Type -> Identifier -> Offset -> ASM ()
saveVar typ name offset = do
    maxOff <- getsFuture _maxOffset
    modifyBackwards (maxOffset +~ offset)
    vars . at name ?= (maxOff, typ)

getUnion :: Identifier -> ASM [Var]
getUnion name = use (structs . at name) >>= \case
    Nothing -> fail $ "usage of undeclared union " ++ show name
    Just x  -> pure x

getStruct :: Identifier -> ASM [Var]
getStruct name = fromMaybeM
    (fail $ "usage of undeclared struct " ++ show name)
    (use (structs . at name))

getVar :: Identifier -> ASM VarRecord
getVar name = fromMaybeM
    (fail $ "usage of undeclared variable " ++ show name)
    (use (vars . at name))

getTypedef :: Identifier -> ASM Type
getTypedef name = fromMaybeM
    (fail $ "usage of undefined type alias " ++ show name)
    (use (typedefs . at name))

convert :: Enum a => a -> Integer
convert = fromIntegral . fromEnum

tryLiftV :: (Integer -> Integer -> Integer)
         -> Value -> Value -> Maybe Value
tryLiftV f (I a) (I b) = Just $ I $ f a b
tryLiftV f (C a) (I b) = Just $ I $ f (convert a) b
tryLiftV f (I a) (C b) = Just $ I $ f a (convert b)
tryLiftV f (C a) (C b) = Just $ I $ f (convert b) (convert b)
tryLiftV _ _ _         = Nothing

simplify :: Expression -> Expression
simplify e@(Infix op (Literal lhs) (Literal rhs)) =
    case opTable op of
        Just f  -> maybe e Literal (tryLiftV f lhs rhs)
        Nothing -> e
simplify e@(Infix op lhs rhs)
    | lhs /= lhs' || rhs /= rhs' = simplify (Infix op lhs' rhs')
    | otherwise = e
  where
    lhs' = simplify lhs
    rhs' = simplify rhs
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
sizedReg reg 8    = showReg reg
sizedReg reg size = regs M.! reg M.! size
  where
    regs = M.fromList
        $ [ (RAX, [(4, "eax"), (2, "ax"), (1, "al" )])
          , (RBX, [(4, "ebx"), (2, "bx"), (1, "bl" )])
          , (RCX, [(4, "ecx"), (2, "cx"), (1, "cl" )])
          , (RDX, [(4, "edx"), (2, "dx"), (1, "dl" )])
          , (RSI, [(4, "esi"), (2, "si"), (1, "sil")])
          , (RDI, [(4, "edi"), (2, "di"), (1, "dil")])
          , (RSP, [(4, "esp"), (2, "sp"), (1, "spl")])
          , (RBP, [(4, "ebp"), (2, "bp"), (1, "bpl")])
          ] ++
          [ let reg = "r" ++ show n in
            (read (upper reg), [ (4, reg ++ "d")
                               , (2, reg ++ "w")
                               , (1, reg ++ "b")
                               ])
          | n <- [8..15]
          ]

sizeToSuffix :: Size -> String
sizeToSuffix 1 = "b"
sizeToSuffix 2 = "w"
sizeToSuffix 4 = "l"
sizeToSuffix 8 = "q"

sizedInst :: String -> Size -> String
sizedInst inst size = inst ++ sizeToSuffix size

showValue :: Value -> String
showValue (I x) = show x
showValue (D x) = show x
showValue (C x) = show x
showValue (S x) = show x

sizeof :: Type -> ASM Size
sizeof Char          = pure 1
sizeof Void          = pure 1
sizeof Short         = pure 2
sizeof Int           = pure 4
sizeof Long          = pure 8
sizeof (Pointer _)   = pure 8
sizeof VarArgs       = pure 8
sizeof Float         = pure 4
sizeof Double        = pure 8
sizeof (Enum _)      = sizeof Int
sizeof (Array t s)   = (*s) <$> sizeof t
sizeof (Struct name) = do
    vs <- getStruct name
    sum <$> mapM (sizeof . _t) vs
sizeof (Union name) = do
    vs <- getUnion name
    maximum <$> mapM (sizeof . _t) vs
sizeof (Alias name) = getTypedef name >>= sizeof

findPointer :: Expression -> ASM (Maybe VarRecord)
findPointer (Variable name) = Just <$> getVar name
findPointer (Infix _ lhs rhs) = findPointer lhs >>= \case
    Just typ -> pure $ Just typ
    Nothing  -> findPointer rhs
findPointer _ = pure Nothing

refToRax :: Identifier -> ASM Size
refToRax name = do
    getVar name >>= \case
        (offset, Pointer typ) -> do
            fwrite "movq -{0}(%rbp), %rax" [show offset]
            sizeof typ
        (offset, Array typ s) -> do
            fwrite "movq -{0}(%rbp), %rax" [show offset]
            sizeof typ
        _ -> fail "invalid type argument of unary '*'"

saveToOffset :: Offset -> Type -> ASM ()
saveToOffset offset typ = do
    size <- sizeof typ
    fwrite "{0} %{1} -{2}(%rbp)"
        [sizedInst "mov" size, sizedReg RAX size, show offset]

saveResult :: Identifier -> ASM ()
saveResult name = do
    use (vars . at name) >>= \case
        Nothing -> fail $ "Assignment to an undefined variable " ++ show name
        Just (offset, typ) -> do
            size <- sizeof typ
            fwrite "{0} %{1}, -{2}(%rbp)"
                [sizedInst "mov" size, sizedReg RAX size, show offset]

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

fwrite :: String -> [String] -> ASM ()
fwrite str = write . format str

label :: String -> ASM ()
label str = do
    indent -= 2
    write $ str ++ ":"
    indent += 2

newLabel :: ASM String
newLabel = do
    n <- nlabels <<+= 1
    pure $ format ".L0{0}" [show n]
