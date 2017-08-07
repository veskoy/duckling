-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.BG.Corpus
  ( corpus
  ) where

import Prelude
import Data.String

import Duckling.Duration.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

corpus :: Corpus
corpus = (testContext {lang = BG}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "секунда"
             , "1 секунда"
             , "сек"
             ]
  , examples (DurationData 30 Second)
             [ "30 секунди"
             , "30сек"
             , "30 сек"
             , "половин минута"
             , "тридесет секунди"
             ]
  , examples (DurationData 1 Minute)
             [ "минута"
             , "минутка"
             , "мин"
             , "1 минута"
             , "1 мин"
             , "1мин"
             ]
  , examples (DurationData 30 Minute)
             [ "половин час"
             , "30 минути"
             , "тридесет минути"
             , "30 мин"
             , "30мин"
             ]
  , examples (DurationData 1 Hour)
             [ "час"
             , "1 час"
             , "един час"
             , "1ч"
             ]
  , examples (DurationData 2 Hour)
             [ "2 часа"
             , "2 ч"
             , "2ч"
             , "два часа"
             ]
  , examples (DurationData 1 Day)
             [ "1 ден"
             , "ден"
             ]
  , examples (DurationData 30 Day)
             [ "30 дни"
             ]
  , examples (DurationData 1 Week)
             [ "1 седмица"
             , "една седмица"
             ]
  , examples (DurationData 7 Week)
             [ "7 седмици"
             , "седем седмици"
             ]
  , examples (DurationData 1 Month)
             [ "1 месец"
             , "месец"
             ]
  , examples (DurationData 6 Month)
             [ "6 месеца"
             , "половин година"
             ]
  , examples (DurationData 1 Year)
             [ "1 година"
             , "година"
             , "1 г"
             , "1г"
             ]
  ]
