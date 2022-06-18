module AST where

import Data.Word ( Word64 )

type Identifier = String
data Type = Int_
          | Short_
          | Char_
          | Long_
          | Pointer_ Type (Maybe Integer)
          | Struct_ [Type]
          | Union_ [Type]
          | VarArgs_
          | Void_
          | Typedef_ String
          deriving ( Show, Eq )

data Value = I Integer
           | D Double
           | C Char
           | S String
           | A [Value] deriving ( Show, Eq )

data Var = Var { _t    :: Type
               , _name :: Identifier
               } deriving ( Show )

data Expression = Variable Identifier
                | Op String (Maybe Expression) (Maybe Expression)
                | Application Identifier [Expression]
                | Assignment Expression Expression
                | Constant Value
                deriving ( Show, Eq )

data Statement = If Expression Statement (Maybe Statement)
               | While Expression Statement
               | For (Maybe Expression) (Maybe Expression) (Maybe Expression) Statement
               | Call Expression
               | Return Expression
               | Declaration Var (Maybe Expression)
               | FDeclaration Type Identifier [Type]
               | FDefinition Type Identifier [Var] Statement
               | Goto Identifier
               | Break
               | Continue
               | Label Identifier
               | Block [Statement]
               deriving ( Show )
