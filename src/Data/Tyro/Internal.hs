{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Tyro.Internal where

import Data.Singletons.TH
import Lib.Prelude

$( singletons [d|
     data JSLens a = JSExtract | JSKey a (JSLens a) | JSArray (JSLens a)
                   deriving (Show)
  |])
