{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# ANN module "HLint: ignore Use camelCase" #-}

module ParserTests where

import Test.Framework
import Parser

test_parseAtom = assertEqual (readExpression "Hallo") $ Atom "Hallo"

