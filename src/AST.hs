module AST where

type Identifier = String
data Type = Int
          | Short
          | Char
          | Long
          | Array Type Integer
          | Pointer Type
          | Struct Identifier
          | Union Identifier
          | Enum Identifier
          | VarArgs
          | Void
          | Float
          | Double
          | Alias String
          deriving ( Show, Eq )

data Value = I Integer
           | D Double
           | C Char
           | S String
           deriving ( Show, Eq )

data Var = Var { _t    :: Type
               , _name :: Identifier
               } deriving ( Show, Eq )

data FieldDesignator = FieldDesignator { field :: Maybe Identifier
                                       , value :: Expression
                                       } deriving ( Show, Eq )

data Expression = Variable Identifier
                | Infix String Expression Expression
                | Prefix String Expression
                | Postfix String Expression
                | Application Identifier [Expression]
                | Assignment Expression Expression
                | InitArr [FieldDesignator]
                | Literal Value
                | Field Identifier Identifier
                | Cast Type Expression
                | Sizeof Type
                deriving ( Show, Eq )

data Statement = If Expression Statement (Maybe Statement)
               | While Expression Statement
               | For (Maybe Expression)
                     (Maybe Expression)
                     (Maybe Expression) Statement
               | Expr Expression
               | Return Expression
               | Declaration Var (Maybe Expression)
               | FDeclaration Type Identifier [Type]
               | FDefinition Type Identifier [Var] Statement
               | Goto Identifier
               | Break
               | Continue
               | Label Identifier
               | Block [Statement]
               | StructDeclaration Identifier [Var]
               | UnionDeclaration Identifier [Var]
               | EnumDeclaration Identifier [Identifier]
               | Typedef Type String
               deriving ( Show )
