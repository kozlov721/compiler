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

import Control.Lens hiding (snoc, Empty)
import Control.Monad.State
import Control.Monad.Tardis
import Control.Monad.Writer
import Data.Char            ( ord, chr )
import Data.Map             ( Map )
import Text.Format
import Text.Pretty.Simple   ( pPrint )
import Data.List.Extra ( unsnoc, snoc )
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)

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

infixl 4 <@>
(<@>) :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
f <@> m = mapM_ f m

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
sizeof VarArgs_ = 64

saveResult :: Identifier -> ASM ()
saveResult name = do
    use (vars . table . at name) >>= \case
        Nothing -> error $ "Assignment to an undefined variable " ++ show name
        Just (n, size)  -> fwrite "{0} %{1}, -{2}(%rbp)"
            [sizedInst "mov" size, sizedReg "rax" size, show n]

withReg :: String -> ASM a -> ASM a
withReg reg m = do
    fwrite "push %{0}" [reg]
    a <- m
    fwrite "pop %{0}" [reg]
    pure a

fwrite :: String -> [String] -> ASM ()
fwrite str = write . format str

evaluate :: Expression -> ASM ()
evaluate (Constant arr@(A _)) = do
    lbl <- gets $ M.size . _globals
    let name = format ".LC{0}" [show lbl]
    globals . at name ?= arr
    fwrite "lea {0}(%rip), %rax" [name]
evaluate (Constant c) = fwrite "mov ${0}, %rax" [showValue c]

evaluate (Variable name) = do
    use (vars . table . at name) >>= \case
        Nothing -> error $ "Usage of undefined variable " ++ show name
        Just (o, size) -> fwrite "{0} -{1}(%rbp), %{2}"
            [sizedInst "mov" size, show o, sizedReg "rax" size]

evaluate (Assignment (Variable name) e) = do
    evaluate e
    saveResult name

evaluate (Op op (Just lhs) (Just rhs))
    | op `elem` ["&&", "||"] = withReg "rcx" $ do
        second <- newLabel
        end <- newLabel

        evaluate lhs
        write "cmp $0, %rax"
        write "mov $0, %rax"
        when (op == "||") $ do
            fwrite "je {0}" [second]
            write "mov $1, %rax"
            fwrite "jmp {0}" [end]
        when (op == "&&") $ do
            fwrite "jne {0}" [second]
            write "mov $0, %rax"
            fwrite "jmp {0}" [end]
        label second
        evaluate rhs
        label end
    | otherwise = withReg "rcx" $ do
        evaluate rhs
        write "push %rax"
        evaluate lhs
        write "pop %rcx"
        when (op `elem` ["==", "!=", ">=", "<=", ">", "<"]) $ do
            write "cmp %rcx, %rax"
            write "mov $0, %rax"
            when (op == "==") (write "sete %al" )
            when (op == "!=") (write "setne %al")
            when (op == "<" ) (write "setl %al" )
            when (op == ">" ) (write "setg %al" )
            when (op == ">=") (write "setge %al")
            when (op == "<=") (write "setle %al")
        when (op == "+") (write "add %rcx, %rax")
        when (op == "-") (write "sub %rcx, %rax")
        when (op == "|") (write "or %rcx, %rax")
        when (op == "&") (write "and %rcx, %rax")
        when (op == "^") (write "xor %rcx, %rax")
        when (op == "*") (write "imul %rcx")
        when (op == "/" || op == "%") $ withReg "rdx" $ do
            write "mov $0, %rdx"
            write "idiv %ecx"
            when (op == "%") (write "mov %rdx, %rax")

evaluate (Op op Nothing (Just x@(Variable name))) = do
    evaluate x
    when (op == "--") (write "dec %rax")
    when (op == "++") (write "inc %rax")
    saveResult name

evaluate (Op op Nothing (Just e)) = do
    evaluate e
    when (op == "!") $ do
        write "cmp $0, %rax"
        write "mov $0, %rax"
        write "sete %al"

evaluate (Application name args) = do
    use (funs . at name) >>= \case
        Nothing -> fail $ "attempt to call an undeclared function " ++ show name
        Just types -> do
            usedRegs <- passArgs
                ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
                (zip (types ++ repeat VarArgs_) args)
            write "mov $0, %rax"
            fwrite "call {0}" [name]
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
        fwrite "push %{0}" [r]
        fwrite "{0} %{1}, %{2}"
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
    write $ str ++ ":"
    indent += 2

newLabel :: ASM String
newLabel = do
    n <- nlabels <<+= 1
    pure $ format ".L0{0}" [show n]

generate :: Statement -> ASM ()
generate (Return e) = do
    evaluate e
    write "leave"
    write "ret"

generate (FDeclaration t name args) = do
    use (funs . at name) >>= \case
        Just _ -> fail $ "multiple declarations of function " ++ show name
        Nothing -> funs . at name ?= args

generate (FDefinition t name args body) = do
    record <- use $ funs . at name
    when (isNothing record) (generate (FDeclaration t name (_t <$> args)))
    write $ ".globl " ++ name
    write $ name ++ ":"

    vs <- use vars
    vars .= empty
    indent += 4

    totOffset <- getsFuture _totVarSize
    -- note: `enter` is slow and obsolete, but I don't care here;
    -- it's worht the few extra lines saved when in need to
    -- debug the produced assembly
    fwrite "enter ${0}, $0" [show totOffset]
    fillArgs ["rdi", "rsi", "rdx", "rcx", "r8", "r9"] args
    generate body

    totSize <- use $ vars . maxOffset
    modifyBackwards (totVarSize .~ totSize)
    vars .= vs
    when (t == Void_) $ generate $ Return $ Constant $ I 0
    indent -= 4
    write ""
  where
    fillArgs :: [String] -> [Var] -> ASM ()
    fillArgs _ [] = pure ()
    fillArgs (r:rs) (v:vs) = do
        generate (Declaration v Nothing)
        Just (o, size) <- use (vars . table . at (_name v))
        fwrite "{0} %{1}, -{2}(%rbp)" [sizedInst "mov" size, sizedReg r size, show o]

generate (Declaration (Var t name) rhs) = do
    use (vars . table . at name) >>= \case
        Just _ -> error $ "Variable " ++ show name ++ " already declared."
        Nothing -> do
            let o = sizeof t `div` 8
            newOffset <- vars . maxOffset <+= o
            vars . table . at name ?= (newOffset, sizeof t)
    case rhs of
        Nothing -> pure ()
        Just e  -> evaluate $ Assignment (Variable name) e

generate (If cond ifBranch elseBranch) = do
    evaluate cond
    write "cmpl $0, %eax"
    els <- newLabel
    end <- newLabel
    fwrite "je {0}" [els]
    generate ifBranch
    fwrite "jmp {0}" [end]
    label els
    generate <@> elseBranch
    label end

generate (While cond body) = do
    begin <- newLabel
    end <- newLabel
    (prevBegin, prevEnd) <- loop <<.= (begin, end)
    label begin
    evaluate cond
    write "cmpl $0, %eax"
    fwrite "je {0}" [end]
    generate body
    fwrite "jmp {0}" [begin]
    label end
    loop .= (prevBegin, prevEnd)

generate (For ini c upd body@(Block b)) = do
    evaluate <@> ini
    let cond = fromMaybe (Constant (I 1)) c
    generate $ While cond $ case upd of
        Just u -> Block (b `snoc` Call u)
        _ -> body

generate (Call e) = evaluate e
generate (Block b) = generate <@> b
generate (Label name) = label name
generate (Goto name) = fwrite "jmp {0}" [name]
generate Continue = use (loop . _1) >>= write . format "jmp {0}" . (:[])
generate Break    = use (loop . _2) >>= write . format "jmp {0}" . (:[])

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
        fwrite ".string {0}" [showValue value]
        indent -= 4

compile :: String -> String
compile str = case parseProgram str of
    Left err -> error $ show err
    Right ast -> execWriter (evalTardisT (toAsm ast) (empty, empty))
