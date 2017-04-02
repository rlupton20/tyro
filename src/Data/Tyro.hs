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
  -- $introduction
  
  -- * Building types
  -- $typed_example
  Extract
, type( >%> )
, List
, unwrap

-- * Value level API
-- $value_example
, Tyro
, extract
, (>%>)
, (%%>)

-- * Internal types
, JSBranch ) where


import           Data.Aeson ((.:))
import qualified Data.Aeson as A
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as B
import           Data.Reflection (reifySymbol)
import           Data.Singletons (Sing, SingI(..))
import           Data.Singletons.Prelude.List (Sing(SNil, SCons))
import           Data.Singletons.TypeLits ( Symbol, SSymbol, KnownSymbol
                                          , withKnownSymbol, symbolVal )
import           Data.String (String)
import           Data.Text (pack)

import           Lib.Prelude



--------------------------------------------------------------------------------
-- Type level API using a type family to mirror JSON structure
--------------------------------------------------------------------------------

-- | @Extract a@ represents trying to parse JSON to an @a@.
type Extract a = JSBranch '[] a

-- | The type operator '>%> provides a way of describing how to walk
-- down a JSON tree.
type family (x :: Symbol) >%> (b :: *) :: *
type instance (x :: Symbol) >%> JSBranch xs a = JSBranch (x ': xs) a

-- | The 'List' type operator constructs a parsing type for parsing
-- a list of JSON objects.
type family List (x :: *) :: *
type instance List (JSBranch xs a) = Extract [JSBranch xs a]



--------------------------------------------------------------------------------
-- Value level API and reification
--------------------------------------------------------------------------------

-- | 'Tyro' is an abstract type representing a parser that walks down a JSON
-- tree.
newtype Tyro = Tyro [String] deriving (Eq, Show)

-- | 'extract' is the value which represents halting the walk along the JSON
-- tree, and pulling out the value there.
extract :: Tyro
extract = Tyro []


-- | '>%>' allows you to specify a subtree indexed by a key. It's right
-- associative, so chains of keys can be specified without parenthesese.
(>%>) :: String -> Tyro -> Tyro
(>%>) s (Tyro t) = Tyro (s:t)
infixr 9 >%>


-- | Internal proxying datatype for accumulating reified values as a list
data TyroProxy :: [Symbol] -> * where
  Take :: TyroProxy '[]
  Key :: TyroProxy s -> TyroProxy (t ': s)
  

-- | '%%>' tries to parse a ByteString along a 'Tyro' to obtain a value
(%%>) :: (A.FromJSON a) => B.ByteString -> Tyro -> Maybe a
(%%>) bs (Tyro xs) = go bs (reverse xs) Take
  where
    go :: (A.FromJSON a, SingI xs) => B.ByteString -> [String] -> TyroProxy xs -> Maybe a
    go b [] t = fmap unwrap $ doParse b t
    go b (k:ks) t = reifySymbol k $ \p -> go b ks (extend t p)

    doParse :: (A.FromJSON a, SingI xs) => B.ByteString -> TyroProxy xs -> Maybe (JSBranch xs a)
    doParse b _ = A.decode b

    extend :: (KnownSymbol s) => TyroProxy xs -> Proxy s -> TyroProxy (s ': xs)
    extend t _ = Key t
infixl 8 %%>



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
-- Documentation
--------------------------------------------------------------------------------

-- $introduction
-- 'Tyro' provides a type driven way of obtaining simple JSON parsers, and
-- a simple value driven interface to obtain values deep inside a JSON object.

-- $typed_example
-- A small (artificial) example demonstrating how to use the typed interface.
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
-- > parsed = decode json :: Maybe ("key1" >%> List ("key2" >%> Extract Integer))
-- >
-- > -- We can dispose of the types using unwrap: 'values' will have the value
-- > -- Just [41, 42]
-- > values :: Maybe [Integer]
-- > values = fmap unwrap parsed


-- $value_example
-- The value level interface allows a piece of the JSON object to be extracted
-- in a similar way to most dynamically typed languages.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Data.Tyro
-- >
-- > json = "{\"key1\": {\"key2\" :  [41, 42]}}"
-- >
-- > -- Extract [41, 42] inside the JSON
-- > parsed = json %%> "key1" >%> "key2" >%> extract :: Maybe [Integer]
--
-- Not the overloaded strings extension in the above is only used to define
-- the 'json' 'ByteString'..
