{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
-- |
-- This module provides functions to modify 'Symbol's and type-level lists.
--
-- The workflow to define a modification function can be as follows.
--
-- 1. Use 'ToList' to convert a 'Symbol' to a type-level list of 'Char's.
--
-- 1. Use functions from here and from the package [first-class-families](https://hackage.haskell.org/package/first-class-families) for type-level lists (@Fcf.Data.List@).
--
-- 1. Use 'FromList' to convert a type-level list of 'Char's back to a 'Symbol'.
--
-- Example (see 'DropPrefix'):
--
-- >>> :kind! DropPrefix "_userParams_category"
-- DropPrefix "_userParams_category" :: Symbol
-- = "category"
--
-- Modules that use the 'Eval' type family (e.g., "Servant.Record") must be imported together with modules that export instances of 'Eval'
-- (see the @GHC@ documentation on Type families).
module Servant.TypeLevel
  ( -- * Re-exports from @first-class-families@
    Eval,
    Exp,

    -- * Helper list functions
    ToList,
    ToList1,
    FromList,
    FromList1,

    -- * Helper comparison functions
    NotTyEq,

    -- * Examples
    DropUnderscores,
    DropNonUnderscores,
    DropPrefix,
  )
where

import Fcf
import Fcf.Data.List
import GHC.TypeLits

-- | Convert a 'Symbol' to a list of 'Char's.
type family ToList (sym :: Symbol) :: [Char] where
  ToList sym = ToList1 (UnconsSymbol sym)

-- | Convert a possibly unconsed 'Symbol' to a list of 'Char's
type family ToList1 (sym :: Maybe (Char, Symbol)) :: [Char] where
  ToList1 'Nothing = '[]
  ToList1 ('Just '(c, sym)) = c : ToList1 (UnconsSymbol sym)

-- | Convert a list of 'Char's to a 'Symbol'.
--
-- >>> :kind! FromList ['a', '+', 'c']
-- FromList ['a', '+', 'c'] :: Symbol
-- = "a+c"
type family FromList (cs :: [Char]) :: Symbol where
  FromList cs = FromList1 (Eval (Reverse cs)) ""

-- | Convert a list of 'Char's to a 'Symbol'.
--
-- In this list, 'Chars' go in reverse order.
--
-- >>> :kind! FromList1 ['a', 'b', 'c'] ""
-- FromList1 ['a', 'b', 'c'] "" :: Symbol
-- = "cba"
type family FromList1 (syms :: [Char]) (sym :: Symbol) :: Symbol where
  FromList1 '[] s = s
  FromList1 (x : xs) s = FromList1 xs (ConsSymbol x s)

-- $ lists

-- | Type inequality
data NotTyEq :: a -> b -> Exp Bool

type instance Eval (NotTyEq a b) = NotTyEqImpl a b

-- | Check types aren't equal
type family NotTyEqImpl (a :: k) (b :: k) :: Bool where
  NotTyEqImpl a a = 'False
  NotTyEqImpl a b = 'True

-- $ examples

-- | Drop leading underscores.
--
-- >>> :kind! Eval (DropUnderscores ['b','_', '_', 'a'])
-- Eval (DropUnderscores ['_', '_', 'a']) :: [Char]
-- = '['a']
type DropUnderscores = DropWhile (TyEq '_')

-- | Drop leading non-underscores.
--
-- >>> :kind! Eval (DropNonUnderscores ['a', 'a', '_'])
-- Eval (DropNonUnderscores ['a', 'a', '_']) :: [Char]
-- = '['_']
type DropNonUnderscores = DropWhile (NotTyEq '_')

-- | Drop the prefix of a 'Symbol'.
--
-- >>> :kind! DropPrefix "_userParams_category"
-- DropPrefix "_userParams_category" :: Symbol
-- = "category"
type family DropPrefix (sym :: Symbol) :: Symbol where
  DropPrefix sym = FromList (Eval (DropUnderscores (Eval (DropNonUnderscores (Eval (DropUnderscores (ToList sym)))))))
