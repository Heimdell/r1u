
module AST where

import Unifiable
import Variable
import Free

import Data.Hashable
import Data.Deriving

import GHC.Generics
import Generics.Deriving.TH

import Fix

newtype Name = Name { unName :: String }
  deriving newtype (Eq, Ord, Show, Hashable)

newtype KName = KName { unName :: String }
  deriving newtype (Eq, Ord, Show, Hashable)

data Type_ self
  = TConst Name
  | TApp self self
  | self :-> self
  | TAlias Name self
  deriving stock (Functor, Foldable, Traversable, Generic)

data Kind_ self
  = Star
  | self :=> self
  deriving stock (Functor, Foldable, Traversable, Generic)

deriveAll1 ''Type_

deriving stock instance Eq a => Eq (Type_ a)
deriving stock instance Ord a => Ord (Type_ a)
deriving stock instance Show a => Show (Type_ a)

deriveAll1 ''Kind_

deriving stock instance Eq a => Eq (Kind_ a)
deriving stock instance Ord a => Ord (Kind_ a)
deriving stock instance Show a => Show (Kind_ a)

instance Variable Name  where gensym i = Name  $ "a" ++ show i
instance Variable KName where gensym i = KName $ "a" ++ show i
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
