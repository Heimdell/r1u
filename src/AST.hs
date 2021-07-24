
module AST where

import Unifiable
import Free

import Data.Hashable
import Data.String

import GHC.Generics
import Generics.Deriving.TH

newtype Name  = Name  { unName :: String } deriving newtype (Eq, Ord, Show, Hashable, IsString)
newtype KName = KName { unName :: String } deriving newtype (Eq, Ord, Show, Hashable, IsString)

data Type_ self
  = TConst Name
  | TApp self self
  | self :-> self
  | TAlias Name self
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

data Kind_ self
  = Star
  | self :=> self
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

deriveAll1 ''Type_
deriveAll1 ''Kind_

deriving anyclass instance Unifiable Type_
deriving anyclass instance Unifiable Kind_

data QType
  = Forall [Name] (Free Type_ Name)
  deriving stock (Eq, Ord, Show)

type Kind   = Free Kind_ KName
type Type   = Free Type_ Name
type Type'  = Maybe Type
type QType' = Maybe QType

data Prog
  = Var    Name
  | Lam    Name  Type' Prog
  | App    Prog  Prog
  | Let   [Decl] Prog
  | Is     Prog  Type'
  | Label  Name
  | Match  Prog [Alt]
  | Const  Constant
  deriving stock (Eq, Ord, Show)

data Decl
  = Bind Name QType' Prog
  | Data Name Kind [Ctor]
  deriving stock (Eq, Ord, Show)

data Ctor
  = Ctor Name QType
  deriving stock (Eq, Ord, Show)

data Alt
  = Alt Pattern Prog Prog
  deriving stock (Eq, Ord, Show)

data Pattern
  = IsVar      Name
  | IsLabel    Name [Pattern]
  | IsConst    Constant
  | IsWildcard
  deriving stock (Eq, Ord, Show)

data Constant
  = I Integer
  | F Float
  | S String
  deriving stock (Eq, Ord, Show)
