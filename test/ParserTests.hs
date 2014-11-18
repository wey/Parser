{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# ANN module "HLint: ignore Use camelCase" #-}

module ParserTests where

import           Parser
import           Test.Framework
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Error

test_parseAtom = assertEqual (readExpression "Hallo") $ Right (Atom "Hallo")

test_parsePlainString = assertEqual (readExpression "\"Hallo\"") $ Right (String "Hallo")

test_parseStringWithEscapedQuote = assertEqual (readExpression "\"Halli\\\"Galli\"") $ Right (String "Halli\"Galli")

test_parseStringWithEscapedNewline = assertEqual (readExpression "\"Halli\\nGalli\"") $ Right (String "Halli\nGalli")

test_parseStringWithEscapedTab = assertEqual (readExpression "\"Halli\\tGalli\"") $ Right (String "Halli\tGalli")

test_parseStringWithEscapedBackslash = assertEqual (readExpression "\"Halli\\\\Galli\"") $ Right (String "Halli\\Galli")

test_parseLowercaseCharacter = assertEqual (readExpression "#\\a") $ Right (Character 'a')

test_parseUppercaseCharacter = assertEqual (readExpression "#\\X") $ Right (Character 'X')

test_parseSpaceCharacter = assertEqual (readExpression "#\\space") $ Right (Character ' ')

test_parseNewlineCharacter = assertEqual (readExpression "#\\newline") $ Right (Character '\n')

test_parseBooleanTrue = assertEqual (readExpression "#t") $ Right (Bool True)

test_parseBooleanFalse = assertEqual (readExpression "#f") $ Right (Bool False)

test_parseInteger = assertEqual (readExpression "123") $ Right (Number 123)

test_parseFloat = assertEqual (readExpression "123.456") $ Right (FloatingPoint 123.456)

test_parseFloat2 = assertEqual (Right $ FloatingPoint 123) $ readExpression "123."

test_parseFloat3 = assertEqual (Right $ FloatingPoint 0.456) $ readExpression ".456"

instance Eq ParseError where
    (==) p1 p2 = errorMessages p1 == errorMessages p2
