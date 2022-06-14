{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}


module Compiler where

import Control.Lens hiding (Empty)
import Control.Monad.State
import Control.Monad.Tardis
import Control.Monad.Writer
import Data.Char            ( ord )
import Data.Map             ( Map )
import Parser
import Text.Format
import Text.Pretty.Simple   ( pPrint )

import qualified Data.Map as M
import Data.Maybe (isJust, fromJust, fromMaybe)

type Offset = Int
data VarsTable = VarsTable { _table     :: Map String (Offset, Type)
                           , _maxOffset :: Int
                           }

data FwState = FwState { _vars   :: VarsTable
                       , _indent :: Int
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
                  , _indent = 0
                  }

instance Empty BwState where
  empty = BwState { _totVarSize = 0
                  }

instance (MonadFix m) => MonadState s (TardisT bw s m) where
  get = getPast
  put = sendFuture

instance (MonadFix m) => MonadFail (TardisT bw fw m) where
  fail = error "fail"

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

getValue :: Value -> String
getValue (I x) = show x
getValue (D x) = show x
getValue (C x) = show x

getMov :: Type -> (String, String)
getMov Char_  = ("movb", "al")
getMov Short_ = ("movw", "ax")
getMov Int_   = ("movl", "eax")
getMov Long_  = ("mov", "rax")


evaluate :: Expression -> ASM ()
evaluate (Constant c) = do
    write $ "mov $" ++ getValue c ++ ", %rax"

evaluate (Variable name) = do
    use (vars . table . at name) >>= \case
        Nothing -> error $ "Usage of undefined variable \"" ++ name ++ "\""
        Just (o, t) -> write $ format "{0} -{1}(%rbp), %{2}" [mov, show o, reg]
          where (mov, reg) = getMov t

evaluate (Assignment (Variable name) e) = do
    evaluate e
    use (vars . table . at name) >>= \case
        Nothing -> error $ "Assignment to an undefined variable " ++ show name
        Just (n, t)  -> write $ format "{0} %{1}, -{2}(%rbp)" [mov, reg, show n]
          where (mov, reg) = getMov t

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

evaluate (Application f args) = do
        passArgs ["rdi", "rsi", "rdx", "rcx", "r8", "r9"] args
        write $ format "call {0}" [f]
  where
    passArgs :: [String] -> [Expression] -> ASM ()
    passArgs _ [] = pure ()
    passArgs [] (e:es) = do
        evaluate e
        write "push %rax"
        passArgs [] es
    passArgs (r:rs) (e:es) = do
        evaluate e
        write $ format "mov %rax, %{0}" [r]
        passArgs rs es


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

generate (FDefinition t name args body) = do
    -- generate <@> map (`Declaration` Nothing) args
    write $ ".globl " ++ name
    write $ name ++ ":"
    indent += 4
    write "push %rbp"
    write "mov %rsp, %rbp"
    totOffset <- getsFuture _totVarSize
    write $ format "sub ${0}, %rsp" [show totOffset]
    fillArgs ["rdi", "rsi", "rdx", "rcx", "r8", "r9"] args
    generate body
    indent -= 4
  where
    fillArgs :: [String] -> [Var] -> ASM ()
    fillArgs _ [] = pure ()
    fillArgs (r:rs) (v:vs) = do
        generate (Declaration v Nothing)
        Just (o, t) <- use (vars . table . at (_name v))
        -- let (mov, reg) = getMov t
        write $ format "mov %{0}, -{1}(%rbp)" [r, show o]

generate (Declaration (Var t name) rhs) = do
    use (vars . table . at name) >>= \case
        Just _ -> error $ "Variable " ++ show name ++ " already declared."
        Nothing -> do
            let o = sizeof t
            newOffset <- vars . maxOffset <+= o
            vars . table . at name ?= (newOffset, t)
    case rhs of
        Nothing -> pure ()
        Just e  -> evaluate $ Assignment (Variable name) e
  where
    sizeof Long_  = 8
    sizeof Int_   = 4
    sizeof Short_ = 2
    sizeof Char_  = 1

generate (Call e) = evaluate e

generate (Block b) = do
    generate <@> b
    totSize <- gets $ maximum . (0:) . map (fst . snd) . M.toList . _table . _vars
    modifyBackwards (totVarSize .~ totSize)

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

toAsm :: FilePath -> IO ()
toAsm path = do
    Right ast <- parseProgram <$> readFile path
    let asm = execWriter (evalTardisT (generate ast) (empty, empty))
    pPrint asm
    let asmFile = (init . init) path ++ ".s"
    writeFile asmFile asm
