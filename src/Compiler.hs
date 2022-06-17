{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns  #-}

module Compiler ( compile ) where

import Parser
import Syntax
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

import qualified Data.Map as M
import Control.Applicative (liftA2)

tryLift :: (Integer -> Integer -> Integer) -> Value -> Value -> Maybe Value
tryLift f (I a) (I b) = Just $ I $ f a b
tryLift f (C a) (I b) = Just $ I $ f (toInteger (fromEnum a)) b
tryLift f (I a) (C b) = Just $ I $ f a (toInteger (fromEnum b))
tryLift f (C a) (C b) = Just $ I $ f (toInteger  (fromEnum b)) (toInteger (fromEnum b))
tryLift _ _ _ = Nothing

simplify :: Expression -> Expression
simplify e@(Op op (Just (Constant lhs)) (Just (Constant rhs))) =
    case opTable op of
        Just f -> maybe e Constant (tryLift f lhs rhs)
        Nothing -> e
simplify e@(Op op lhs rhs)
    | lhs /= lhs' || rhs /= rhs' = simplify (Op op lhs' rhs')
    | otherwise = e
  where
    lhs' = simplify <$> lhs
    rhs' = simplify <$> rhs
simplify (Assignment lhs rhs) = Assignment (simplify lhs) (simplify rhs)
simplify e = e

evaluate :: Expression -> ASM ()
evaluate = evaluate' . simplify

evaluate' :: Expression -> ASM ()
evaluate' (Constant arr@(A _)) = do
    lbl <- gets $ M.size . _globals
    let name = format ".LC{0}" [show lbl]
    globals . at name ?= arr
    fwrite "lea {0}(%rip), %rax" [name]
evaluate' (Constant c) = fwrite "mov ${0}, %rax" [showValue c]

evaluate' (Variable name) = do
    use (vars . table . at name) >>= \case
        Nothing -> error $ "Usage of undefined variable " ++ show name
        Just (o, size) -> fwrite "{0} -{1}(%rbp), %{2}"
            [sizedInst "mov" size, show o, sizedReg RAX size]

evaluate' (Assignment (Variable name) e) = do
    evaluate e
    saveResult name

evaluate' (Op op (Just lhs) (Just rhs))
    | op `elem` ["&&", "||"] = withReg "rcx" $ do
        second <- newLabel
        end <- newLabel

        evaluate lhs
        write "cmp $0, %rax"
        write "mov $0, %rax"
        case op of
            "||" -> do
                fwrite "je {0}" [second]
                write "mov $1, %rax"
                fwrite "jmp {0}" [end]
            "&&" -> do
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
            case op of
                "==" -> write "sete %al"
                "!=" -> write "setne %al"
                "<"  -> write "setl %al"
                ">"  -> write "setg %al"
                ">=" -> write "setge %al"
                "<=" -> write "setle %al"
        case op of
            "+" -> write "add %rcx, %rax"
            "-" -> write "sub %rcx, %rax"
            "|" -> write "or %rcx, %rax"
            "&" -> write "and %rcx, %rax"
            "^" -> write "xor %rcx, %rax"
            "*" -> write "imul %rcx"
            _   -> pure ()
        when (op == "/" || op == "%") $ withReg "rdx" $ do
            write "mov $0, %rdx"
            write "idiv %ecx"
            when (op == "%") (write "mov %rdx, %rax")

evaluate' (Op op Nothing (Just x@(Variable name))) = do
    evaluate' x
    when (op == "--") (write "dec %rax")
    when (op == "++") (write "inc %rax")
    saveResult name

evaluate' (Op op Nothing (Just e)) = do
    evaluate' e
    when (op == "!") $ do
        write "cmp $0, %rax"
        write "mov $0, %rax"
        write "sete %al"

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
        evaluate e
        write "push %rax"
        passArgs [] es
    passArgs (r:rs) ((t, e):es) = do
        -- write $ "# " ++ show e
        evaluate e
        let size = sizeof t
        fwrite "push %{0}" [showReg r]
        fwrite "{0} %{1}, %{2}"
            [sizedInst "mov" size, sizedReg RAX size, sizedReg r size]
        (r:) <$> passArgs rs es

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
    fillArgs _ [] = pure ()
    fillArgs (r:rs) (v:vs) = do
        generate (Declaration v Nothing)
        Just (o, size) <- use (vars . table . at (_name v))
        fwrite "{0} %{1}, -{2}(%rbp)"
            [sizedInst "mov" size, sizedReg r size, show o]

generate (Declaration (Var t name) rhs) = do
    use (vars . table . at name) >>= \case
        Just _  -> error $ "Variable " ++ show name ++ " already declared."
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
        _      -> body

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
    Left err  -> error $ show err
    Right ast -> execWriter (evalTardisT (toAsm ast) (empty, empty))
