{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Test.Text.DraCor.Concomitance

main = htfMain htf_importedTests
