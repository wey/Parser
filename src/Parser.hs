{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser where

import Data.List
import Text.ParserCombinators.Parsec hiding (spaces)

evaluateExpression :: String -> String
evaluateExpression expression = case readExpression expression of
    Left errorMessage -> "Input did not match: " ++ show errorMessage
    Right match -> "Found a match: " ++ show match

readExpression :: String -> Either ParseError LispValue
readExpression = parse parseExpression "lisp"
    
parseExpression :: Parser LispValue
parseExpression = try parseString 
              <|> try parseBool
              <|> try parseFloat
              <|> try parseNumber
              <|> try parseCharacter
              <|> try parseAtom
    

parseString :: Parser LispValue
parseString = do
    char '"'
    content <- many validChar
    char '"'
    return $ String content
    where
        validChar = tryChar '"' '"' <|> tryChar 'n' '\n' <|> tryChar 't' '\t' <|> tryChar '\\' '\\' <|> noneOf "\""
        tryChar character replacement = try $ escapedChar character replacement
        
escapedChar :: Char -> Char -> Parser Char
escapedChar character returnCharacter = do
    char '\\'
    char character
    return returnCharacter

parseAtom :: Parser LispValue
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    return $ Atom (first : rest)

parseBool :: Parser LispValue
parseBool = try parseTrue <|> try parseFalse where
    parseTrue = parseBool' 't' True
    parseFalse = parseBool' 'f' False
    parseBool' character returnValue = do
        hash
        char character
        return $ Bool returnValue    
    
parseNumber :: Parser LispValue
parseNumber = do
    digits <- many1 digit
    let number = read digits
    return $ Number number 

parseFloat :: Parser LispValue
parseFloat = do
    preCommaDigits <- many digit
    char '.'
    afterCommaDigits <- many digit
    let number = read (pad preCommaDigits ++ "." ++ pad afterCommaDigits) where
        pad digits = if null digits then "0" else digits 
    return $ FloatingPoint number 

parseCharacter :: Parser LispValue
parseCharacter = do
    charPrefix
    spaceChar <|> newlineChar <|> singleChar where
        spaceChar = do
            string "space" 
            return $ Character ' '
        newlineChar = do
            string "newline" 
            return $ Character '\n'
        singleChar = do
            character <- anyChar
            return $ Character character

charPrefix :: Parser Char
charPrefix = do
    hash
    char '\\'

hash :: Parser Char
hash = char '#'

symbol :: Parser Char
symbol = oneOf "!$%&*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space


data LispValue = Atom String
               | List [LispValue]
               | DottedList [LispValue] LispValue
               | Number Integer
               | String String
               | Character Char
               | FloatingPoint Float
               | Bool Bool


instance Show LispValue where
    show (Atom value) = "Atom: " ++ value
    show (List values) = "[" ++ intercalate ", " (map show values) ++ "]"
    show (DottedList values lastValue) = "[" ++ intercalate ", " (map show values) ++ " . " ++ show lastValue ++ "]"
    show (Number n) = "Number: " ++ show n
    show (String s) = "String: " ++ s
    show (Character c) = "Character: " ++ show c
    show (FloatingPoint f) = "Float: " ++ show f
    show (Bool b) = "Bool: " ++ show b
    
instance Eq LispValue where
    (==) (Atom v1) (Atom v2) = v1 == v2
    (==) (List l1) (List l2) = l1 == l2
    (==) (DottedList l1 lv1) (DottedList l2 lv2) = l1 == l2 && lv1 == lv2
    (==) (Number n1) (Number n2) = n1 == n2 
    (==) (String s1) (String s2) = s1 == s2
    (==) (Character c1) (Character c2) = c1 == c2
    (==) (FloatingPoint f1) (FloatingPoint f2) = f1 == f2
    (==) (Bool b1) (Bool b2) = b1 == b2
    (==) _ _ = False
    
    
    
    
    
    



