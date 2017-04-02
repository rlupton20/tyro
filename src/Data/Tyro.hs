{-|
Module      : Data.Tyro
Description : A library for automatically deriving JSON parsers from types
Copyright   : (c) Richard Lupton, 2017
License     : BSD-3
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Data.Tyro (
  -- * Introduction
  -- $example

  -- * Building types
  Parse
, type( |>| )
, List
, unwrap

-- * Internal types
, JSBranch ) where


import           Data.Aeson ((.:))
import qualified Data.Aeson as A
import           Data.Aeson.Types (Parser)
import           Data.Singletons (Sing, SingI(..))
import           Data.Singletons.Prelude.List (Sing(SNil, SCons))
import           Data.Singletons.TypeLits ( Symbol, SSymbol, KnownSymbol
                                          , withKnownSymbol, symbolVal )
import           Data.String (String)
import           Data.Text (pack)

import           Lib.Prelude

import Data.Reflection (reifySymbol)
import qualified Data.ByteString.Lazy as B


-- $example
-- A small (artificial) example demonstrating how to use the types defined here.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE TypeOperators #-}
-- > import Data.Tyro
-- > import Data.Aeson (decode)
-- > import qualified Data.ByteString.Lazy as B
-- >
-- > json = "{\"key1\":[{\"key2\":41},{\"key2\":42}]}" :: B.ByteString
-- >
-- > -- Extract [41, 42] inside the Tyro types
-- > parsed = decode json :: Maybe ("key1" |>| List ("key2" |>| Parse Integer))
-- >
-- > -- We can dispose of the types using unwrap: 'values' will have the value
-- > -- Just [41, 42]
-- > values :: Maybe [Integer]
-- > values = fmap unwrap parsed



--------------------------------------------------------------------------------
-- Type level API using a type family to mirror JSON structure
--------------------------------------------------------------------------------

-- | @Parse a@ represents trying to parse JSON to an @a@.
type Parse a = JSBranch '[] a

-- | The type operator '|>|' provides a way of describing how to walk
-- down a JSON tree.
type family (x :: Symbol) |>| (b :: *) :: *
type instance (x :: Symbol) |>| JSBranch xs a = JSBranch (x ': xs) a

-- | The 'List' type operator constructs a parsing type for parsing
-- a list of JSON objects.
type family List (x :: *) :: *
type instance List (JSBranch xs a) = Parse [JSBranch xs a]


--------------------------------------------------------------------------------
-- Basic dependent structure
--------------------------------------------------------------------------------

-- | 'JSBranch' is a dependent datatype which represents a walk down a JSON
-- tree. @JSBranch ["key1", "key2"] a@ represents the walk "take the value at
-- @key1@ and then the value at @key2@, and (try to) interpret that as an @a@".
data JSBranch :: [Symbol] -> * -> * where
  JSNil :: a -> JSBranch '[] a
  JSCons :: JSBranch xs a -> JSBranch (t ': xs) a


-- | 'unwrap' unwraps a value from it's parsing type.
unwrap :: JSBranch xs a -> a
unwrap b = case b of
  JSNil x -> x
  JSCons b' -> unwrap b'


instance (A.FromJSON a, SingI xs) => A.FromJSON (JSBranch xs a) where
  parseJSON :: (A.FromJSON a, SingI xs) => A.Value -> Parser (JSBranch xs a)
  parseJSON = parseSing sing
    where
      parseSing :: (A.FromJSON a) => Sing xs -> A.Value -> Parser (JSBranch xs a)
      parseSing s o = case s of
        SNil -> JSNil <$> A.parseJSON o
        x `SCons` xs -> case o of
          A.Object v -> let key = pack (reflectSym x) in
            JSCons <$> (v .: key >>= parseSing xs)
          _ -> empty


-- | 'reflectSym' reflects a type level symbol into a value level string
reflectSym :: SSymbol s -> String
reflectSym s = withKnownSymbol s $ proxySym s Proxy
  where
    proxySym :: (KnownSymbol n) => SSymbol n -> Proxy n -> String
    proxySym _ = symbolVal



--------------------------------------------------------------------------------
-- Value level API and reification
--------------------------------------------------------------------------------

data Tyro = Take | Key String Tyro deriving (Eq, Show)

data TyroD :: [Symbol] -> * where
  TakeD :: TyroD '[]
  KeyD :: TyroD s -> TyroD (t ': s)
  

parse :: (A.FromJSON a) => B.ByteString -> Tyro -> Maybe a
parse b t = go b t TakeD
  where
    go :: (A.FromJSON a, SingI xs) => B.ByteString -> Tyro -> TyroD xs -> Maybe a
    go b Take t = fmap unwrap $ doParse b t
    go b (Key k r) t = reifySymbol k $ \p -> go b r (extend t p)

    doParse :: (A.FromJSON a, SingI xs) => B.ByteString -> TyroD xs -> Maybe (JSBranch xs a)
    doParse b _ = A.decode b

    extend :: (KnownSymbol s) => TyroD xs -> Proxy s -> TyroD (s ': xs)
    extend t _ = KeyD t
