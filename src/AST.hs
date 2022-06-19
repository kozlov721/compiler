module AST where

type Identifier = String
data Type = Int_
          | Short_
          | Char_
          | Long_
          | Array_ Type (Maybe Int)
          | Pointer_ Type
          | Struct_ Identifier
          | Union_ Identifier
          | Enum_ Identifier
          | VarArgs_
          | Void_
          | Float_
          | Double_
          deriving ( Show, Eq )

data Value = I Int
           | D Double
           | C Char
           | S String
           deriving ( Show, Eq )

data Var = Var { _t    :: Type
               , _name :: Identifier
               } deriving ( Show, Eq )

data Expression = Variable Identifier
                | Op String (Maybe Expression) (Maybe Expression)
                | Index Identifier Expression
                | Application Identifier [Expression]
                | Assignment Expression Expression
                | InitArr [Expression]
                | Constant Value
                | Field Identifier Identifier
                | Cast Type Expression
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
               | Struct Identifier [Var]
               deriving ( Show )
