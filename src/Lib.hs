module Lib
    ( someFunc
    ) where

import System.IO
import System.Directory
import Control.Exception
import System.FilePath

notesDir :: String
notesDir = "hasnotes"
dir = ""

printHelp :: IO()
printHelp = do
    putStrLn "Here is a list of commands to use the notes app!\n"
    putStrLn "--help, --h \t\t These show this help prompt!"
    putStrLn "--ldr \t\t list directory where your notes are saved!"
    parseInput


-- Function to get the notes directory path in home directory
getNotesDir :: IO FilePath
getNotesDir = do
 homeDir <- getHomeDirectory
 return $ homeDir </> "hasnotes"

-- Function to ensure the notes directory exists
ensureNotesDir :: IO ()
ensureNotesDir = do
 notesDir <- getNotesDir
 exists <- doesDirectoryExist notesDir
 unless exists $ do
     createDirectory notesDir
     putStrLn $ "Created directory: " ++ notesDir

-- Function to list directory contents
listNotesDirectory :: IO ()
listNotesDirectory = do
 ensureNotesDir
 notesDir <- getNotesDir
 contents <- listDirectory notesDir
 if null contents
   then putStrLn $ "Directory '" ++ notesDir ++ "' is empty."
   else do
     putStrLn $ "Contents of '" ++ notesDir ++ "' directory:"
     mapM_ putStrLn contents
 parseInput

parseInput :: IO()
parseInput = do
  putStr "\x1b[32m > \x1b[0m "
  hFlush stdout
  input <- getLine
  case input of
    "--help" -> printHelp
    "--h"    -> printHelp
    "exit"   -> return ()
    "--ldr"  -> listNotesDirectory
    _        -> parseInput

someFunc :: IO ()
someFunc = do
    putStrLn "Welcome to your notes!, for a list of commands type --help or --h"
    parseInput
    putStrLn "Exiting, Goodbye!"

