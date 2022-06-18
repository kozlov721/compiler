{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Parser where

import AST

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String

import Text.Pretty.Simple ( pPrint )

import Data.Char    ( isAlpha, toUpper )
import Data.Functor ( ($>) )
import Data.Void    ( Void )

import qualified Text.Parsec.Token as P


langDef = emptyDef
    { P.commentStart    = "/*"
    , P.commentEnd      = "*/"
    , P.commentLine     = "//"
    , P.reservedNames   = [ "return" , "if" , "else" , "while"
                          , "for" , "void" , "char" , "int"
                          , "double" , "break" , "continue"
                          , "goto" , "sizeof", "long", "short"
                          ]
    , P.reservedOpNames =
          let simple = [ "+" , "-" , "*" , "/"
                       , ">>" , "<<" , "&" , "|" , "^"
                       ]
          in simple ++ [ "--" , "++" , "==" , "!=", "||" , "&&"
                       , ">"  , "<" , "<=" , ">=" , "~" , "!"
                       ] ++ map ("="++) simple
    }

lexer = P.makeTokenParser langDef

lexeme        = P.lexeme lexer
parens        = P.parens lexer
braces        = P.braces lexer
brackets      = P.brackets lexer
identifier    = P.identifier lexer
reserved      = P.reserved lexer
reservedOp    = P.reservedOp lexer
operator      = P.operator lexer
symbol        = P.symbol lexer
dot           = P.dot lexer
colon         = P.colon lexer
semi          = P.semi lexer
whiteSpace    = P.whiteSpace lexer
float         = P.float lexer
stringLiteral = P.stringLiteral lexer
charLiteral   = P.charLiteral lexer
integer       = P.integer lexer
commaSep      = P.commaSep lexer
semiSep       = P.semiSep lexer

type_ :: Parser Type
type_ = do
    t <- choice [ reserved "int" $> Int_
                , reserved "short" $> Short_
                , reserved "char" $> Char_
                , reserved "long" $> Long_
                , reserved "void" $> Void_
                , lexeme (string "...") $> VarArgs_
                ]
    choice [ reservedOp "*" $> Pointer_ t Nothing
           , Pointer_ t <$> (Just <$> brackets integer)
           , pure t
           ]

var :: Parser Var
var = do
    t <- type_
    if   t == VarArgs_
    then pure (Var VarArgs_ "...")
    else Var t <$> identifier

val :: Parser Value
val = choice [ try $ D <$> float
             , I . fromInteger <$> integer
             , C <$> charLiteral
             , S <$> stringLiteral
             , A <$> braces (commaSep val)
             ]

term :: Parser Expression
term = choice [ Constant <$> val
              , do
                  name <- identifier
                  option (Variable name)
                         (Application name <$> parens (commaSep expr))
              , Variable <$> identifier
              , parens expr
              ]

expr :: Parser Expression
expr =  buildExpressionParser table term
    <|> term
  where
    table = [ [ prefix "++"
              , prefix "--"
              , prefix "*"
              , prefix "!"
              , prefix "~"
              , prefix "&"
              ]
            , [ postfix "++"
              , postfix "--"
              ]
            , [ binary "*" AssocLeft
              , binary "/" AssocLeft
              , binary "%" AssocLeft
              ]
            , [ binary "+" AssocLeft
              , binary "-" AssocLeft
              ]
            , [ binary ">>" AssocLeft
              , binary "<<" AssocLeft
              ]
            , [ binary "&" AssocLeft ]
            , [ binary "|" AssocLeft ]
            , [ binary op AssocNone
              | op <- ["<=", ">=", ">", "<"]
              ]
            , [ binary "==" AssocLeft
              , binary "!=" AssocLeft
              ]
            , [ binary "&&" AssocLeft ]
            , [ binary "||" AssocLeft ]
            , [ assignMod op AssocRight
              | op <- [ "+=", "-=", "*=", "/=", "%="
                      , "&=", "|=", "^=", ">>=", "<<=" ]
              ]
            , [ assign AssocNone ]
            ]
    infixOp op l r = Op op (Just l) (Just r)
    prefixOp op  r = Op op Nothing (Just r)
    postfixOp op l = Op op (Just l) Nothing
    assign         = Infix $ try (reservedOp "=") $> Assignment
    assignMod name = Infix $ do
        try $ reservedOp name
        let op = init name
        pure $ \lhs rhs -> Assignment lhs $ Op op (Just lhs) (Just rhs)
    binary name     = Infix   $ try (reservedOp name) $> infixOp name
    prefix name     = Prefix  $ try (reservedOp name) $> prefixOp name
    postfix name    = Postfix $ try (reservedOp name) $> prefixOp name

semiStatement :: Parser Statement
semiStatement = choice
    [ reserved "return" >> Return <$> expr
    , reserved "goto" >> Goto <$> identifier
    , reserved "break" $> Break
    , reserved "continue" $> Continue
    , Call <$> expr
    , Declaration <$> try var <*> optionMaybe (try (reservedOp "=" >> expr))
    ] <* semi

statement :: Parser Statement
statement = semiStatement <|> choice
    [ reserved "while" >> While <$> parens expr <*> block
    , do
        reserved "if"
        cond <- parens expr
        ifB <- block <|> statement
        elseB <- optionMaybe (try (reserved "else") >> (block <|> statement))
        pure $ If cond ifB elseB
    , do
        reserved "for"
        parens (semiSep (optionMaybe (try expr))) >>= \case
            [ini, cond, upd] -> For ini cond upd <$> block
            _                -> fail "ill-formed 'for' declaration"
    , Label <$> identifier <* colon
    ]

topLevel :: Parser Statement
topLevel = do
    t <- type_
    name <- identifier
    choice $ map try
        [ do
            args <- parens (commaSep var)
            FDefinition t name args <$> block
        , do
            types <- parens (commaSep (type_ <* optional identifier))
            FDeclaration t name types <$ semi
        ]

block :: Parser Statement
block = Block <$> (braces . many . lexeme) statement

program :: Parser Statement
program = whiteSpace >> Block <$> many (lexeme topLevel) <* eof

parseProgram :: String -> Either ParseError Statement
parseProgram = parse program ""
