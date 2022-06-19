{-# LANGUAGE LambdaCase #-}

module Compiler ( compile ) where

import AST
import Parser
import Utils

import Control.Monad.State
import Control.Monad.Tardis
import Control.Monad.Writer

import Control.Lens    hiding ( Empty, snoc )
import Data.Char       ( chr, ord )
import Data.List.Extra ( snoc, unsnoc )
import Data.Map        ( Map )
import Data.Maybe      ( fromJust, fromMaybe, isJust, isNothing )

import Text.Format
import Text.Pretty.Simple ( pPrint )

import           Control.Applicative ( liftA2 )
import qualified Data.Map            as M


evaluate :: Expression -> ASM ()
evaluate = evaluate' . simplify

evaluate' :: Expression -> ASM ()
evaluate' (Constant str@(S _)) = do
    lbl <- gets $ M.size . _globals
    let name = format ".LC{0}" [show lbl]
    globals . at name ?= str
    fwrite "lea {0}(%rip), %rax" [name]

evaluate' (Constant c) = fwrite "mov ${0}, %rax" [showValue c]

evaluate' (Variable name) = do
    (o, t) <- getVar name
    let size = sizeof t
    fwrite "{0} -{1}(%rbp), %{2}"
        [sizedInst "mov" size, show o, sizedReg RAX size]

evaluate' (Assignment (Variable name) (Constant (A arr))) = do
    (o, Array_ t s) <- getVar name
    saveArr (arr ++ repeat (last arr)) (sizeof t) o
  where
    saveArr :: [Value] -> Size -> Offset -> ASM ()
    saveArr _ _ 0 = pure ()
    saveArr (x:xs) size o = do
        let bSize = sizeToBytes size
        evaluate (Constant x)
        fwrite "{0} %{1}, -{2}(%rbp)"
            [sizedInst "mov" size, sizedReg RAX size, show o]
        saveArr xs size (o - bSize)
    saveArr _ _ _ = error "something went wrong, missaligned offset"

evaluate' (Assignment (Variable name) e) = do
    evaluate e
    saveResult name

evaluate' (Reference (Variable name)) = do
    (o, t) <- getVar name
    let size = sizeof t
    fwrite "leaq -{0}(%rbp), %rax" [show o]

evaluate' (Dereference (Variable name)) = do
    size <- refToRax name
    fwrite "{0} (%rax), %{1}" [sizedInst "mov" size, sizedReg RAX size]

evaluate' (Dereference e) = findType e >>= \case
    Nothing -> fail "invalid type argument of unary '*'"
    Just t -> do
        let size = sizeof t
        evaluate e
        fwrite "{0} (%rax), %{1}" [sizedInst "mov" size, sizedReg RAX size]

evaluate' (Assignment d@(Dereference (Variable name)) e) = withReg RCX $ do
    evaluate e
    write "mov %rax, %rcx"
    size <- refToRax name
    fwrite "{0} %{1}, (%rax)" [sizedInst "mov" size, sizedReg RCX size]

evaluate' (Binary op lhs rhs)
    | op `elem` ["&&", "||"] = withReg RCX $ do
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
    | otherwise = withReg RCX $ do
        evaluate rhs
        write "push %rax"
        evaluate lhs
        write "pop %rcx"
        when (op `elem` ["==", "!=", ">=", "<=", ">", "<"]) $ do
            write "cmp %rcx, %rax"
            write "mov $0, %rax"
            when (op == "==") (write "sete %al")
            when (op == "!=") (write "setne %al")
            when (op == "<") (write "setl %al")
            when (op == ">") (write "setg %al")
            when (op == ">=") (write "setge %al")
            when (op == "<=") (write "setle %al")
        when (op == "+") (write "add %rcx, %rax")
        when (op == "-") (write "sub %rcx, %rax")
        when (op == "|") (write "or %rcx, %rax")
        when (op == "&") (write "and %rcx, %rax")
        when (op == "^") (write "xor %rcx, %rax")
        when (op == "*") (write "imul %rcx")
        when (op == "/" || op == "%") $ withReg RDX $ do
            write "mov $0, %rdx"
            write "idiv %ecx"
            when (op == "%") (write "mov %rdx, %rax")

evaluate' (Prefix op x@(Variable name)) = do
    evaluate' x
    when (op == "--") (write "dec %rax")
    when (op == "++") (write "inc %rax")
    saveResult name

evaluate' (Prefix op e) = do
    evaluate' e
    when (op == "!") $ do
        write "cmp $0, %rax"
        write "mov $0, %rax"
        write "sete %al"
    when (op == "~") (write "not %rax")

evaluate' (Application name args) = do
    use (funs . at name) >>= \case
        Nothing -> fail
            $ "attempt to call an undeclared function " ++ show name
        Just types -> do
            usedRegs <- passArgs argRegs
                (zip (types ++ repeat VarArgs_) args)
            write "mov $0, %rax"
            fwrite "call {0}" [name]
            write . format "pop %{0}" . (:[]) <@> map showReg usedRegs
  where
    passArgs :: [Register] -> [(Type, Expression)] -> ASM [Register]
    passArgs _ [] = pure []
    passArgs [] ((t, e):es) = do
        -- last argument must be pushed first
        reg <- passArgs [] es
        evaluate e
        write "push %rax"
        pure reg
    passArgs (r:rs) ((t, e):es) = do
        evaluate e
        let size = sizeof t
        fwrite "push %{0}" [showReg r]
        fwrite "{0} %{1}, %{2}"
            [sizedInst "mov" size, sizedReg RAX size, sizedReg r size]
        (r:) <$> passArgs rs es
evaluate' x = fail $ show x

generate :: Statement -> ASM ()
generate (Return e) = do
    evaluate e
    write "leave"
    write "ret"

generate (FDeclaration t name args) = do
    use (funs . at name) >>= \case
        Just _  -> fail $ "multiple declarations of function " ++ show name
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
    fillArgs argRegs args
    generate body

    totSize <- use $ vars . maxOffset
    modifyBackwards (totVarSize .~ totSize)
    vars .= vs
    when (t == Void_) $ generate $ Return $ Constant $ I 0
    indent -= 4
    write ""
  where
    fillArgs :: [Register] -> [Var] -> ASM ()
    fillArgs [] (v:vs) = fail
        "More than five arguments not currently supported"
    fillArgs _ [] = pure ()
    fillArgs (r:rs) (v:vs) = do
        generate (Declaration v Nothing)
        Just (o, t) <- use (vars . table . at (_name v))
        let size = sizeof t
        fwrite "{0} %{1}, -{2}(%rbp)"
            [sizedInst "mov" size, sizedReg r size, show o]

generate (Declaration (Var arrT@(Array_ itemT size) name) rhs) = do
    checkUndeclared name
    case rhs of
        Just rhs@(Constant (A arr)) -> do
            let s = fromMaybe (length arr) size
            let o = s * sizeToBytes (sizeof itemT)
            saveVar arrT name o
            evaluate $ Assignment (Variable name) rhs
        Just _ -> fail "rhs of an array definition must be a literal array"
        Nothing -> case size of
            Nothing -> fail "undeclared array without specified size"
            Just s  -> do
                let o = s * sizeToBytes (sizeof itemT)
                saveVar arrT name o

generate (Declaration (Var t name) rhs) = do
    checkUndeclared name
    let o = sizeToBytes $ sizeof t
    saveVar t name o
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
        _      -> body

generate (Call e) = evaluate e
generate (Block b) = generate <@> b
generate (Label name) = label name
generate (Goto name) = fwrite "jmp {0}" [name]
generate Continue = use (loop . _1) >>= write . format "jmp {0}" . (:[])
generate Break    = use (loop . _2) >>= write . format "jmp {0}" . (:[])
generate x =  fail $ show x

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
    Left err  -> error $ show err
    Right ast -> execWriter (evalTardisT (toAsm ast) (empty, empty))
