{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# ANN module "HLint: ignore Use camelCase" #-}

module ParserTests where

import           Parser
import           Test.Framework
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Error

test_parseAtom = assertEqual (Right $ Atom "Hallo") $ readExpression "Hallo"

test_parsePlainString = assertEqual (Right $ String "Hallo") $ readExpression "\"Hallo\""

test_parseStringWithEscapedQuote = assertEqual (Right $ String "Halli\"Galli") $ readExpression "\"Halli\\\"Galli\""

test_parseStringWithEscapedNewline = assertEqual (Right $ String "Halli\nGalli") $ readExpression "\"Halli\\nGalli\""

test_parseStringWithEscapedTab = assertEqual (Right $ String "Halli\tGalli") $ readExpression "\"Halli\\tGalli\""

test_parseStringWithEscapedBackslash = assertEqual (Right $ String "Halli\\Galli") $ readExpression "\"Halli\\\\Galli\""

test_parseLowercaseCharacter = assertEqual (Right $ Character 'a') $ readExpression "#\\a"

test_parseUppercaseCharacter = assertEqual (Right $ Character 'X') $ readExpression "#\\X"

test_parseSpaceCharacter = assertEqual (Right $ Character ' ') $ readExpression "#\\space"

test_parseNewlineCharacter = assertEqual (Right $ Character '\n') $ readExpression "#\\newline"

test_parseBooleanTrue = assertEqual (Right $ Bool True) $ readExpression "#t"

test_parseBooleanFalse = assertEqual (Right $ Bool False) $ readExpression "#f"

test_parseInteger = assertEqual (Right $ Number 123) $ readExpression "123" 

test_parseFloat = assertEqual (Right $ FloatingPoint 123.456) $ readExpression "123.456"

test_parseFloat2 = assertEqual (Right $ FloatingPoint 123) $ readExpression "123."

test_parseFloat3 = assertEqual (Right $ FloatingPoint 0.456) $ readExpression ".456"

instance Eq ParseError where
    (==) p1 p2 = errorMessages p1 == errorMessages p2
