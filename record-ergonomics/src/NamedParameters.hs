{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module NamedParameters where

import VanillaRecords (Bar (..))

import Named (arg, (!), type (:!))


newBar ::
    "a" :! Int ->
    "d" :! Int ->
    "e" :! Int ->
    Bar
newBar (arg #a -> ba) (arg #d -> bd) (arg #e -> be) =
    Bar{ba, bb = Nothing, bc = 123, bd, be}


bar :: Bar
bar = newBar ! #a 1 ! #d 2 ! #e 3
