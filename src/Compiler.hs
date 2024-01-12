{-# LANGUAGE LambdaCase #-}

module Compiler ( compile ) where

import AST
import Parser ( parseProgram )
import Utils

import Control.Applicative  ( liftA2 )
import Control.Monad.State
import Control.Monad.Tardis
import Control.Monad.Writer
import Data.Char            ( chr, ord )
import Data.List.Extra      ( snoc, unsnoc )
import Data.Map             ( Map )
import Data.Maybe           ( fromJust, fromMaybe, isJust, isNothing )
import Text.Format
import Text.Pretty.Simple   ( pPrint )

import Control.Lens hiding ( Empty, snoc )

import qualified Data.Map as M

saveArr :: [(Type, Expression)] -> Offset -> ASM ()
saveArr [] _ = pure ()
saveArr ((typ, e):es) offset = do
    size <- sizeof typ
    evaluate e
    fwrite "{0} %{1}, -{2}(%rbp)"
        [sizedInst "mov" size, sizedReg RAX size, show offset]
    saveArr es (offset - size)

evaluate :: Expression -> ASM ()
evaluate = evaluate' . simplify

evaluate' :: Expression -> ASM ()
evaluate' (Literal str@(S _)) = do
    lbl <- gets $ M.size . _globals
    let name = format ".LC{0}" [show lbl]
    globals . at name ?= str
    fwrite "lea {0}(%rip), %rax" [name]

evaluate' (Literal c) = fwrite "mov ${0}, %rax" [showValue c]

evaluate' (Variable name) = do
    (offset, typ) <- getVar name
    size <- sizeof typ
    fwrite "{0} -{1}(%rbp), %{2}"
        [sizedInst "mov" size, show offset, sizedReg RAX size]

-- evaluate' (Index name e) = withReg RCX $ do
--     (offset, Array typ _) <- getVar name
--     evaluate e
--     size <- sizeof typ
--     fwrite "{0} -{1}(%rbp, %rax, {2}), %{3}"
--         [ sizedInst "mov" size, show offset
--         , show $ sizeToBytes size, sizedReg RAX size]
--
-- evaluate' (Assignment (Index name i) e) = withReg RCX $ do
--     (offset, Array typ _) <- getVar name
--     evaluate i
--     size <- sizeof typ
--     fwrite "leaq -{0}(%rbp, %rax, {1}), %rax"
--         [ show offset , show $ sizeToBytes size]
--     write "push %rax"
--     evaluate e
--     write "pop %rcx"
--     fwrite "{0} %{1}, (%rcx)" [sizedInst "mov" size, sizedReg RAX size]

evaluate' (Assignment (Variable name) (InitArr arr)) = do
    (offset, Array typ size) <- getVar name
    let len = fromIntegral $ length arr
    when (len > size) (fail "excess elements in array initializer")
    let paddedArr = arr ++ replicate (fromInteger (size - len)) (last arr)
    if   all (isNothing . field) paddedArr
    then saveArr (zip (repeat typ) (map value paddedArr)) offset
    else fail "attemt to initialize array with struct initializer"

evaluate' (Assignment (Variable name) e) = do
    evaluate e
    saveResult name

evaluate' (Reference (Variable name)) = do
    (offset, typ) <- getVar name
    size <- sizeof typ
    fwrite "leaq -{0}(%rbp), %rax" [show offset]

-- evaluate' (Reference (Index name e)) = do
--     evaluate e
--     (offset, Array typ _) <- getVar name
--     size <- sizeof typ
--     fwrite "leaq -{0}(%rbp, %rax, {1}), %rax"
--         [show offset, show size]

evaluate' (Dereference (Variable name)) = do
    size <- refToRax name
    getVar name >>= \case
        (_, Pointer _) -> fwrite "{0} (%rax), %{1}"
            [sizedInst "mov" size, sizedReg RAX size]
        _ -> pure ()

-- evaluate' (Dereference e) = findType e >>= \case
--     Nothing -> fail "invalid type argument of unary '*'"
--     Just typ -> do
--         size <- sizeof typ
--         evaluate e
--         fwrite "{0} (%rax), %{1}" [sizedInst "mov" size, sizedReg RAX size]

evaluate' (Assignment (Dereference (Variable name)) e) = withReg RCX $ do
    evaluate e
    write "mov %rax, %rcx"
    size <- refToRax name
    fwrite "{0} %{1}, (%rax)" [sizedInst "mov" size, sizedReg RCX size]

evaluate' (Infix op lhs rhs)
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

evaluate' (Prefix op v@(Variable name)) = do
    evaluate' v
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
        Just (retType, argTypes) -> do
            usedRegs <- passArgs argRegs
                (zip (argTypes ++ repeat VarArgs) args)
            write "mov $0, %rax"
            fwrite "call {0}" [name]
            write . format "pop %{0}" . (:[]) <@> map showReg usedRegs
  where
    passArgs :: [Register] -> [(Type, Expression)] -> ASM [Register]
    passArgs _ [] = pure []
    passArgs [] ((_, e):es) = do
        -- last argument must be pushed first
        reg <- passArgs [] es
        evaluate e
        write "push %rax"
        pure reg
    passArgs (reg:regs) ((typ, e):es) = do
        evaluate e
        size <- sizeof typ
        fwrite "push %{0}" [showReg reg]
        fwrite "{0} %{1}, %{2}"
            [sizedInst "mov" size, sizedReg RAX size, sizedReg reg size]
        (reg:) <$> passArgs regs es
evaluate' x = fail $ show x

generate :: Statement -> ASM ()
generate (Return e) = do
    evaluate e
    write "leave"
    write "ret"

generate (FDeclaration returnType name argTypes) = do
    use (funs . at name) >>= \case
        Just _  -> fail $ "multiple declarations of function " ++ show name
        Nothing -> funs . at name ?= (returnType, argTypes)

generate (FDefinition typ name args (Block body)) = do
    record <- use $ funs . at name
    when (isNothing record) (generate (FDeclaration typ name (_t <$> args)))
    write $ ".globl " ++ name
    write $ name ++ ":"

    vs <- use vars
    vars .= mempty
    indent += 4

    totOffset <- getsFuture _maxOffset
    -- note: `enter` is slow and obsolete, but I don't care;
    -- it's worth the few lines saved when in need to
    -- debug the produced assembly
    fwrite "enter ${0}, $0" [show totOffset]
    fillArgs argRegs args
    when (null body || notReturn (last body)) $
        generate $ Return $ Literal $ I 0
    generate <@> body
    vars .= vs
    indent -= 4
    write ""
  where
    notReturn (Return _) = False
    notReturn _ = True

    fillArgs :: [Register] -> [Var] -> ASM ()
    fillArgs [] (v:vs) = fail
        "More than five arguments not currently supported"
    fillArgs _ [] = pure ()
    fillArgs (r:rs) (v:vs) = do
        generate (Declaration v Nothing)
        Just (o, typ) <- use (vars . at (_name v))
        size <- sizeof typ
        fwrite "{0} %{1}, -{2}(%rbp)"
            [sizedInst "mov" size, sizedReg r size, show o]

generate (Declaration (Var arrT@(Array itemT arrSize) name) rhs) = do
    checkUndeclared name
    case rhs of
        Just rhs@(InitArr arr) -> do
            itemSize <- sizeof itemT
            let offset = arrSize * itemSize
            saveVar arrT name offset
            void $ evaluate $ Assignment (Variable name) rhs
        Just _ -> fail "rhs of an array definition must be a literal array"
        Nothing -> do
            offset <- (arrSize*) <$> sizeof itemT
            saveVar arrT name offset

generate (Declaration (Var typ name) rhs) = do
    checkUndeclared name
    size <- sizeof typ
    saveVar typ name size
    case rhs of
        Nothing -> pure ()
        Just e  -> void $ evaluate $ Assignment (Variable name) e

generate (StructDeclaration name fields) = do
    use (structs . at name) >>= \case
        Just _  -> fail $ "multiple declarations of struct " ++ show name
        Nothing -> structs . at name ?= fields

generate (If cond ifBranch elseBranch) = do
    evaluate cond
    write "cmpl $0, %eax"
    elseLabel <- newLabel
    endLabel <- newLabel
    fwrite "je {0}" [elseLabel]
    generate ifBranch
    fwrite "jmp {0}" [endLabel]
    label elseLabel
    generate <@> elseBranch
    label endLabel

generate (While cond body) = do
    beginLabel <- newLabel
    endLabel <- newLabel
    (prevBegin, prevEnd) <- loop <<.= (beginLabel, endLabel)
    label beginLabel
    evaluate cond
    write "cmpl $0, %eax"
    fwrite "je {0}" [endLabel]
    generate body
    fwrite "jmp {0}" [beginLabel]
    label endLabel
    loop .= (prevBegin, prevEnd)

generate (For ini c upd body@(Block block)) = do
    evaluate <@> ini
    let cond = fromMaybe (Literal (I 1)) c
    generate $ While cond $ case upd of
        Just upd -> Block (block `snoc` Expr upd)
        _        -> body

generate (Expr e) = void $ evaluate e
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
