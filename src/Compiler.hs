{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}


module Compiler ( compile ) where

import Parser
import Utils

import Control.Lens hiding (Empty)
import Control.Monad.State
import Control.Monad.Tardis
import Control.Monad.Writer
import Data.Char            ( ord, chr )
import Data.Map             ( Map )
import Text.Format
import Text.Pretty.Simple   ( pPrint )

import qualified Data.Map as M
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)

type Offset = Int
data VarsTable = VarsTable { _table     :: Map String (Offset, Type)
                           , _maxOffset :: Int
                           }

data FwState = FwState { _vars    :: VarsTable
                       , _funs    :: Map Identifier [Type]
                       , _indent  :: Int
                       , _globals :: Map Identifier Value
                       }

data BwState = BwState { _totVarSize :: Int }

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

infixl 4 <@>
(<@>) :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
f <@> m = mapM_ f m


opTable :: Map String String
opTable = M.fromList [ ("+", "add")
                     , ("-", "sub")
                     , ("*", "imul")
                     , ("/", "idiv")
                     ]

showValue :: Value -> String
showValue (I x) = show x
showValue (D x) = show x
showValue (C x) = show x
showValue (A str@((C _):_)) = show $ map (\(C c) -> c) str

sizeof :: Type -> Int
sizeof Char_ = 8
sizeof Short_ = 16
sizeof Int_ = 32
sizeof Long_ = 64
sizeof (Pointer_ _ _) = 64

evaluate :: Expression -> ASM ()
evaluate (Constant arr@(A _)) = do
    lbl <- gets $ M.size . _globals
    let name = format ".L{0}" [show lbl]
    globals . at name ?= arr
    write $ format "lea {0}(%rip), %rax" [name]
evaluate (Constant c) = write $ format "mov ${0}, %rax" [showValue c]

evaluate (Variable name) = do
    use (vars . table . at name) >>= \case
        Nothing -> error $ "Usage of undefined variable \"" ++ name ++ "\""
        Just (o, t) -> write $ format "{0} -{1}(%rbp), %{2}"
            [sizedInst "mov" size, show o, sizedReg "rax" size]
          where size = sizeof t

evaluate (Assignment (Variable name) e) = do
    evaluate e
    use (vars . table . at name) >>= \case
        Nothing -> error $ "Assignment to an undefined variable " ++ show name
        Just (n, t)  -> write $ format "{0} %{1}, -{2}(%rbp)"
            [sizedInst "mov" size, sizedReg "rax" size, show n]
          where size = sizeof t

evaluate (Op op (Just lhs) (Just rhs)) = do
    evaluate lhs
    write "push %rax"
    evaluate rhs
    write "pop %rcx"
    when (op == "+") (write "add %rcx, %rax")
    when (op == "-") (write "sub %rax, %rcx" >> write "mov %rcx, %rax")
    when (op == "*") (write "imul %rcx")
    when (op == "/" || op == "%") $ do
        write "push %rdx"
        write "mov $0, %edx"
        write "idiv %ecx"
        when (op == "%") (write "movl %edx, %eax")
        write "pop %rdx"
    when (op `elem` ["==", "!=", ">=", "<=", ">", "<"]) $ do
        write "cmp %rax, %rcx"
        write "mov $0, %rax"
        when (op == "==") (write "sete %al" )
        when (op == "!=") (write "setne %al")
        when (op == "<" ) (write "setl %al" )
        when (op == ">" ) (write "setg %al" )
        when (op == ">=") (write "setge %al")
        when (op == "<=") (write "setle %al")

evaluate (Application name args) = do
    use (funs . at name) >>= \case
        Nothing -> fail $ "attempt to call an undeclared function " ++ show name
        Just types -> do
            usedRegs <- passArgs
                ["rdi", "rsi", "rdx", "rcx", "r8", "r9"] (zip types args)
            write "mov $0, %rax"
            write $ format "call {0}" [name]
            write . format "pop %{0}" . (:[]) <@> usedRegs
  where
    passArgs :: [String] -> [(Type, Expression)] -> ASM [String]
    passArgs _ [] = pure []
    passArgs [] ((t, e):es) = do
        evaluate e
        write "push %rax"
        passArgs [] es
    passArgs (r:rs) ((t, e):es) = do
        evaluate e
        let size = sizeof t
        write $ format "push %{0}" [r]
        write $ format "{0} %{1}, %{2}"
            [sizedInst "mov" size, sizedReg "rax" size, sizedReg r size]
        (r:) <$> passArgs rs es


write :: String -> ASM ()
write str = do
    n <- getsPast _indent
    lift $ tell $ str `indentBy` n ++ "\n"
  where
    indentBy str n = replicate n ' ' ++ str

label :: String -> ASM ()
label str = do
    indent -= 2
    write str
    indent += 2

generate :: Statement -> ASM ()
generate (Return e) = do
    evaluate e
    write "mov %rbp, %rsp"
    write "pop %rbp"
    write "ret"

generate (FDeclaration t name args) = do
    use (funs . at name) >>= \case
        Just _ -> fail $ "multiple declarations of function " ++ show name
        Nothing -> funs . at name ?= map _t args

generate (FDefinition t name args body) = do
    record <- use $ funs . at name
    when (isNothing record) (generate (FDeclaration t name args))
    write $ ".globl " ++ name
    write $ name ++ ":"

    vs <- use vars
    vars .= empty
    indent += 4

    write "push %rbp"
    write "mov %rsp, %rbp"
    totOffset <- getsFuture _totVarSize
    write $ format "sub ${0}, %rsp" [show totOffset]
    fillArgs ["rdi", "rsi", "rdx", "rcx", "r8", "r9"] args
    generate body

    indent -= 4
    totSize <- use $ vars . maxOffset
    modifyBackwards (totVarSize .~ totSize)
    vars .= vs
    write ""
  where
    fillArgs :: [String] -> [Var] -> ASM ()
    fillArgs _ [] = pure ()
    fillArgs (r:rs) (v:vs) = do
        generate (Declaration v Nothing)
        Just (o, t) <- use (vars . table . at (_name v))
        let size = sizeof t
        write $ format "{0} %{1}, -{2}(%rbp)" [sizedInst "mov" size, sizedReg r size, show o]

generate (Declaration (Var t name) rhs) = do
    use (vars . table . at name) >>= \case
        Just _ -> error $ "Variable " ++ show name ++ " already declared."
        Nothing -> do
            let o = sizeof t `div` 8
            newOffset <- vars . maxOffset <+= o
            vars . table . at name ?= (newOffset, t)
    case rhs of
        Nothing -> pure ()
        Just e  -> evaluate $ Assignment (Variable name) e

generate (Call e) = evaluate e

generate (Block b) = do
    generate <@> b

generate (If cond ifBranch elseBranch) = do
    evaluate cond
    write "cmpl $0, %eax"
    write "jz 1f"
    generate ifBranch
    label "1:"
    generate <@> elseBranch

generate (While cond body) = do
    label "1:"
    evaluate cond
    write "jz 2f"
    generate body
    write "jmp 1b"
    label "2:"

generate (For ini cond upd body) = do
    evaluate <@> ini
    label "1:"
    when (isJust cond) (evaluate <@> cond >> write "jz 2f")
    generate body
    evaluate <@> upd
    write "jmp 1b"
    label "2:"

toAsm :: Statement -> ASM ()
toAsm st = do
    generate st
    globs <- gets $ M.toList . _globals
    unless (null globs) (write ".data" >> writeGlob <@> globs)
  where
    writeGlob :: (Identifier, Value) -> ASM ()
    writeGlob (name, value) = do
        write $ name ++ ":"
        indent += 4
        write $ format ".string {0}" [showValue value]
        indent -= 4

compile :: String -> String
compile str = case parseProgram str of
    Left err -> error $ show err
    Right ast -> execWriter (evalTardisT (toAsm ast) (empty, empty))
