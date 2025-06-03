{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import System.IO
import System.Directory
import Control.Exception
import System.FilePath
import Parser (readMarkdownFile)
import Control.Monad (unless)
import Data.List (sort)
import qualified Data.ByteString as BS

notesDir :: String
notesDir = "hasnotes"
dir = ""

-- ANSI color codes
blueColor :: String
blueColor = "\ESC[34m"

resetColor :: String
resetColor = "\ESC[0m"

printHelp :: IO()
printHelp = do
    putStrLn "Here is a list of commands to use the notes app!\n"
    putStrLn "--help, --h \t\t These show this help prompt!"
    putStrLn "--ldr \t\t list directory where your notes are saved!"
    putStrLn "pn \t\t Print Note, this prints the notes"
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

printTreeItem :: FilePath -> String -> Bool -> (String, Bool) -> IO ()
printTreeItem baseDir prefix isLast (item, isDir) = do
  let fullPath = baseDir </> item
  let connector = if isLast then "└── " else "├── "
  let newPrefix = prefix ++ (if isLast then "    " else "│   ")

  if isDir
    then do
      putStrLn $ prefix ++ connector ++ blueColor ++ item ++ "/" ++ resetColor
      -- List contents of this directory
      subContents <- listDirectory fullPath
      unless (null subContents) $ do
          sortedSubContents <- mapM (\subItem -> do
            let subFullPath = fullPath </> subItem
            subIsDir <- doesDirectoryExist subFullPath
            return (subItem, subIsDir)) subContents
          let sortedItems = sort sortedSubContents
          mapM_ (printTreeItemWithIndex fullPath newPrefix (length sortedItems))
                (zip [1..] sortedItems)
    else putStrLn $ prefix ++ connector ++ item

printTreeItemWithIndex :: FilePath -> String -> Int -> (Int, (String, Bool)) -> IO ()
printTreeItemWithIndex baseDir prefix totalItems (index, itemInfo) =
  printTreeItem baseDir prefix (index == totalItems) itemInfo

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
      sortedContents <- mapM (\item -> do
        let fullPath = notesDir </> item
        isDir <- doesDirectoryExist fullPath
        return (item, isDir)) contents
      mapM_ (printTreeItem notesDir "" True) (sort sortedContents)
  parseInput

isMarkdownFile :: FilePath -> IO Bool
isMarkdownFile filePath = do
  fileExists <- doesFileExist filePath
  if not fileExists
    then return False
    else do
      -- Check extension
      let ext = takeExtension filePath
      let validExtensions = [".md", ".markdown", ".mdown", ".mkd", ".mkdn"]
      if ext `elem` validExtensions
        then do
          -- Check magic bytes (first few bytes of file)
          handle (\(_ :: IOError) -> return False) $ do
            bytes <- BS.take 4 <$> BS.readFile filePath
            -- Simple check: markdown files are typically plain text
            -- We'll check if it doesn't start with common binary signatures
            let notBinary = not (BS.isPrefixOf (BS.pack [0xFF, 0xFE]) bytes ||  -- UTF-16 LE BOM
                                BS.isPrefixOf (BS.pack [0xFE, 0xFF]) bytes ||  -- UTF-16 BE BOM
                                BS.isPrefixOf (BS.pack [0x89, 0x50, 0x4E, 0x47]) bytes || -- PNG
                                BS.isPrefixOf (BS.pack [0xFF, 0xD8, 0xFF]) bytes || -- JPEG
                                BS.isPrefixOf (BS.pack [0x25, 0x50, 0x44, 0x46]) bytes) -- PDF
            return notBinary
        else return False
        
-- Function to print a markdown note
printNote :: String -> IO ()
printNote filename = do
  notesDir <- getNotesDir
  let filePath = notesDir </> filename
  
  -- Try the filename as-is first
  fileExists <- doesFileExist filePath
  actualPath <- if fileExists
    then return filePath
    else do
      -- Try adding .md extension if not present
      let withMdExt = if ".md" `elem` [takeExtension filename]
                      then filePath
                      else filePath <.> "md"
      mdExists <- doesFileExist withMdExt
      if mdExists
        then return withMdExt
        else return filePath -- Return original to show proper error
  
  finalExists <- doesFileExist actualPath
  if not finalExists
    then putStrLn $ "Error: File '" ++ filename ++ "' not found in notes directory."
    else do
      isMarkdown <- isMarkdownFile actualPath
      if not isMarkdown
        then putStrLn $ "Error: '" ++ filename ++ "' is not a valid markdown file."
        else do
          putStrLn $ "Reading note: " ++ takeFileName actualPath
          putStrLn $ replicate 50 '-'
          readMarkdownFile actualPath
          putStrLn $ replicate 50 '-'
  parseInput
-- Parse command with arguments
parseCommand :: String -> IO ()
parseCommand input = 
  case words input of
    ["--help"] -> printHelp
    ["--h"] -> printHelp
    ["exit"] -> return ()
    ["--ldr"] -> listNotesDirectory
    ["pn"] -> do
      putStrLn "Usage: pn <filename>"
      parseInput
    ("pn":args) -> do
      let filename = unwords args
      if null filename
        then do
          putStrLn "Usage: pn <filename>"
          parseInput
        else printNote filename
    _ -> do
      putStrLn "Unknown command. Type --help for available commands."
      parseInput

parseInput :: IO()
parseInput = do
  putStr "\x1b[32m > \x1b[0m "
  hFlush stdout
  input <- getLine
  parseCommand input

someFunc :: IO ()
someFunc = do
    putStrLn "Welcome to your notes!, for a list of commands type --help or --h"
    parseInput
    putStrLn "Exiting, Goodbye!"

