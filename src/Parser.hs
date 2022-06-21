{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Parser where

import AST hiding ( Infix, Prefix, Postfix )

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String

import Text.Pretty.Simple ( pPrint )

import Data.Char    ( isAlpha, toUpper )
import Data.Functor ( ($>) )
import Control.Applicative (liftA2)

import qualified AST
import qualified Text.Parsec.Token as P


langDef = emptyDef
    { P.commentStart    = "/*"
    , P.commentEnd      = "*/"
    , P.commentLine     = "//"
    , P.reservedNames   = [ "return" , "if" , "else" , "while"
                          , "for" , "void" , "char" , "int"
                          , "double", "float" , "break" , "continue"
                          , "goto" , "sizeof", "long", "short", "struct"
                          ]
    , P.reservedOpNames =
          let simple = [ "+" , "-" , "*" , "/"
                       , ">>" , "<<" , "&" , "|" , "^"
                       ]
          in simple ++ [ "--" , "++" , "==" , "!=", "||" , "&&"
                       , ">"  , "<" , "<=" , ">=" , "~" , "!", "->"
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
integer       = fromInteger <$> P.integer lexer
commaSep      = P.commaSep lexer
commaSep1     = P.commaSep1 lexer
semiSep       = P.semiSep lexer

typ :: Parser Type
typ = do
    t <- choice [ reserved "int" $> Int
                , reserved "short" $> Short
                , reserved "char" $> Char
                , reserved "long" $> Long
                , reserved "void" $> Void
                , reserved "float" $> Float
                , reserved "double" $> Double
                , lexeme (string "...") $> VarArgs
                , reserved "struct" $> Struct <*> identifier
                , reserved "enum" $> Enum <*> identifier
                , reserved "union" $> Union <*> identifier
                , Alias <$> identifier
                ]
    option t $ reservedOp "*" $> Pointer t

var :: Parser Var
var = do
    t <- typ
    name <- identifier
    if   t == VarArgs
    then pure (Var VarArgs "...")
    else option (Var t name) $ do
        size <- brackets $ optionMaybe integer
        case size of
            Just size -> pure $ Var (Array t size) name
            Nothing -> do
                arr <- lookAhead
                    $ reservedOp "=" >> braces (commaSep fieldDesignator)
                pure $ Var (Array t (fromIntegral (length arr))) name

val :: Parser Value
val = choice [ try $ D <$> float
             , I <$> integer
             , C <$> charLiteral
             , S <$> stringLiteral
             ]

fieldDesignator :: Parser FieldDesignator
fieldDesignator = do
    field <- optionMaybe $ dot *> identifier <* reservedOp "="
    FieldDesignator field <$> expr


term :: Parser Expression
term = choice [ Literal <$> val
              , do
                  name <- identifier
                  choice [ Field name <$> (dot >> identifier)
                         , Application name <$> parens (commaSep expr)
                         , AST.Infix "[]"
                            <$> option (Variable name) (parens expr)
                            <*> brackets expr
                         , pure $ Variable name
                         ]
              , try $ reserved "sizeof" >> Sizeof <$> parens typ
              , try $ Cast <$> parens typ <*> expr
              , InitArr <$> braces (commaSep fieldDesignator)
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
              , binary "->" AssocNone
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
    assign         = Infix $ try (reservedOp "=") $> Assignment
    assignMod name = Infix $ do
        try $ reservedOp name
        let op = init name
        pure $ \lhs rhs -> Assignment lhs $ AST.Infix op lhs rhs
    binary name  = Infix   $ try (reservedOp name) $> AST.Infix name
    prefix name  = Prefix  $ try (reservedOp name) $> AST.Prefix name
    postfix name = Postfix $ try (reservedOp name) $> AST.Postfix name

semiStatement :: Parser Statement
semiStatement = choice
    [ reserved "return" >> Return <$> expr
    , reserved "goto" >> Goto <$> identifier
    , reserved "break" $> Break
    , reserved "continue" $> Continue
    , Expr <$> expr
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

funs :: Parser Statement
funs = do
    t <- typ
    name <- identifier
    choice $ map try
        [ do
            args <- parens (commaSep var)
            FDefinition t name args <$> block
        , do
            types <- parens (commaSep (typ <* optional identifier))
            FDeclaration t name types <$ semi
        ]

topLevel :: Parser Statement
topLevel = choice [ do
                      reserved "struct"
                      name <- identifier
                      fields <- braces $ many (var <* semi)
                      semi
                      pure $ StructDeclaration name fields
                  , do
                      reserved "union"
                      name <- identifier
                      fields <- braces $ many (var <* semi)
                      semi
                      pure $ UnionDeclaration name fields
                  , do
                      reserved "enum"
                      name <- identifier
                      fields <- braces $ commaSep1 identifier
                      semi
                      pure $ EnumDeclaration name fields
                  , funs
                  ]

block :: Parser Statement
block = Block <$> (braces . many . lexeme) statement

program :: Parser Statement
program = whiteSpace >> Block <$> many (lexeme topLevel) <* eof

parseProgram :: String -> Either ParseError Statement
parseProgram = parse program ""
