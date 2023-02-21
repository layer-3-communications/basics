{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnboxedTuples #-}

module Main
  ( main
  ) where


import Prelude hiding (read)

import Test.Tasty (TestTree,defaultMain,testGroup,adjustOption)

import qualified BitBool as BitBool
import qualified Int1 as Int1
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain
  $ adjustOption (\_ -> QC.QuickCheckTests 4000)
  $ tests

tests :: TestTree
tests = testGroup "basics"
  [ BitBool.tests
  , Int1.tests
  ]
