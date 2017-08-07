-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.BG.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("секунда (grain)"  , "сек(унда|унди)?"    , TG.Second)
         , ("минута (grain)"   , "мин(ута|ути)?"      , TG.Minute)
         , ("час (grain)"      , "ч(ас|аса)?"         , TG.Hour)
         , ("ден (grain)"      , "(ден|дни)"          , TG.Day)
         , ("седмица (grain)"  , "седмиц(а|и)"        , TG.Week)
         , ("месец (grain)"    , "месеци?"            , TG.Month)
         , ("четвърт (grain)"  , "четвърти?"          , TG.Quarter)
         , ("година (grain)"   , "г(од)?(ина|ини)?"   , TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
