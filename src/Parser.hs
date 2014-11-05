{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser(readExpression) where

import Data.List
import Text.ParserCombinators.Parsec hiding (spaces)


readExpression :: String -> String
readExpression expression = case parse parseExpression "lisp" expression of
    Left errorMessage -> "Input did not match: " ++ show errorMessage
    Right match -> "Found a match: " ++ show match
    
parseExpression :: Parser LispValue
parseExpression = parseAtom <|> parseString <|> parseNumber
    

parseString :: Parser LispValue
parseString = do
    char '"' -- leading Quote
    content <- many validChar
    char '"' -- trailing Quote
    return $ String content
    where
        validChar = try escapedQuote <|> noneOf "\""
        escapedQuote = do
            char '\\'
            char '"'
            return '"'

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

-- liftM (Number . read) (many1 digit)


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space


data LispValue = Atom String
               | List [LispValue]
               | DottedList [LispValue] LispValue
               | Number Integer
               | String String
               | Bool Bool


instance Show LispValue where
    show (Atom value) = "Atom: " ++ value
    show (List values) = "[" ++ intercalate ", " (map show values) ++ "]"
    show (DottedList values lastValue) = "[" ++ intercalate ", " (map show values) ++ " . " ++ show lastValue ++ "]"
    show (Number n) = "Number: " ++ show n
    show (String s) = "String: " ++ s
    show (Bool b) = "Bool: " ++ show b
    
    
    
    
    
    
    
    



