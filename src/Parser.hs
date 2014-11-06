{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser(readExpression) where

import Data.List
import Text.ParserCombinators.Parsec hiding (spaces)


readExpression :: String -> String
readExpression expression = case parse parseExpression "lisp" expression of
    Left errorMessage -> "Input did not match: " ++ show errorMessage
    Right match -> "Found a match: " ++ show match
    
parseExpression :: Parser LispValue
parseExpression = try parseString 
              <|> try parseNumber
              <|> try parseFloat
              <|> try parseCharacter
              <|> try parseAtom
    

parseString :: Parser LispValue
parseString = do
    char '"'
    content <- many validChar
    char '"'
    return $ String content
    where
        validChar = try (escaped '"' '"')
                <|> try (escaped 'n' '\n')
                <|> try (escaped 't' '\t')
                <|> try (escaped '\\' '\\') 
                <|> noneOf "\""
        escaped character returnChar = do
            char '\\'
            char character
            return returnChar

parseAtom :: Parser LispValue
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    let atom = first : rest
    let value = case atom of 
            "#t" -> Bool True
            "#f" -> Bool False
            _    -> Atom atom
    return value
    
parseNumber :: Parser LispValue
parseNumber = do
    digits <- many1 digit
    let number = read digits
    return $ Number number 

parseFloat :: Parser LispValue
parseFloat = parseNumber

parseCharacter :: Parser LispValue
parseCharacter = do
    char '#'
    char '\\'
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

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

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
    
    
    
    
    
    
    
    



