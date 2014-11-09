module Main where

import Control.Exception

import Data.List

import System.Directory (getDirectoryContents)
import System.Environment
import System.FilePath (takeExtension)

import Parser(evaluateExpression)



main :: IO ()
main = do
    args <- parseArguments
    result <- case args of
        File path -> parseFile path
        Expression expression -> parseExpression expression
        RunTest -> do
            fileList <- getDirectoryContents "."
            let testFiles = filter checkExtension fileList where
                checkExtension path = takeExtension path ==  ".exp"
            results <- mapM parseFile $ reverse testFiles
            return $ intercalate "\n" results
    putStrLn result
            

parseFile :: FilePath -> IO String
parseFile path = do
    content <- readFile path
    parseExpression content 
    
parseExpression :: String -> IO String
parseExpression expression = return $ evaluateExpression expression    

parseArguments :: IO ProgramAction
parseArguments = do
    args <- getArgs
    case args of
        ["-f", file]-> return $ File file
        ["-e", expression]-> return $ Expression expression
        ["-t"] -> return RunTest
        _ -> throw $ PatternMatchFail "Incorrect Parameters"


data ProgramAction = File FilePath
                   | Expression String
                   | RunTest




