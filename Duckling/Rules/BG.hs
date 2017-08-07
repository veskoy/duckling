-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.BG
  ( rules
  ) where

import Duckling.Dimensions.Types
import qualified Duckling.Duration.BG.Rules as Duration
import qualified Duckling.TimeGrain.BG.Rules as TimeGrain
import Duckling.Types

rules :: Some Dimension -> [Rule]
rules (This Distance) = []
rules (This Duration) = Duration.rules
rules (This Numeral) = []
rules (This Email) = []
rules (This AmountOfMoney) = []
rules (This Ordinal) = []
rules (This PhoneNumber) = []
rules (This Quantity) = []
rules (This RegexMatch) = []
rules (This Temperature) = []
rules (This Time) = []
rules (This TimeGrain) = TimeGrain.rules
rules (This Url) = []
rules (This Volume) = []
