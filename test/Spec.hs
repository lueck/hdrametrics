{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Test.Text.DraCor.FoldPlay
import {-@ HTF_TESTS @-} Test.Text.DraCor.Concomitance
import {-@ HTF_TESTS @-} Test.Text.DraCor.Cooccurrence
import {-@ HTF_TESTS @-} Test.Text.DraCor.Dominance

main = htfMain htf_importedTests
