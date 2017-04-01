{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Data.Jason (
  JSBranch
, unwrap
, Parse
, type( |>| )
, List) where

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


data JSBranch :: [Symbol] -> * -> * where
  JSNil :: a -> JSBranch '[] a
  JSCons :: JSBranch xs a -> JSBranch (t ': xs) a


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


reflectSym :: SSymbol s -> String
reflectSym s = withKnownSymbol s $ proxySym s Proxy
  where
    proxySym :: (KnownSymbol n) => SSymbol n -> Proxy n -> String
    proxySym _ = symbolVal


--------------------------------------------------------------------------------
-- Nicer type level API using a type family to mirror JSON structure
--------------------------------------------------------------------------------

type Parse a = JSBranch '[] a

type family (x :: Symbol) |>| (b :: *) :: *
type instance (x :: Symbol) |>| JSBranch xs a = JSBranch (x ': xs) a

type family List (x :: *) :: *
type instance List (JSBranch xs a) = Parse [JSBranch xs a]

