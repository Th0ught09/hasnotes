module Parser (readMarkdownFile) where

import System.Console.ANSI
import Control.Monad (unless)
import DisplayHeaders

testCases = "#Header1\n##Header2\n###Header3\n####Header4\n#####Header5\n######Header6\ntext"

readMarkdownFile :: FilePath -> IO ()
readMarkdownFile filePath = do
  contents <- readFile filePath
  let contentLines = lines contents
  parseLine contentLines

parseLine :: [String] -> IO()
parseLine [] = return ()
parseLine (x:xs) = do
  let line = x
  if null line
    then parseLine xs
    else do
      let command = head line
      case command of
        '#' -> do
          -- Parse header level and text
          let (level, headerText) = parseHeaderLevel line
          displayHeader level headerText
          parseLine xs
        _ -> do
          -- Handle regular text
          putStrLn line
          parseLine xs

-- Function to determine header level and extract text
parseHeaderLevel :: String -> (Int, String)
parseHeaderLevel line = 
  let hashes = takeWhile (== '#') line
      level = length hashes
      text = dropWhile (== ' ') (drop level line)  -- Remove hashes and leading spaces
  in (min level 6, text)  -- Cap at 6 levels

-- Function to display header with different styles based on level
displayHeader :: Int -> String -> IO ()
displayHeader level text = do
  putStrLn ""
  case level of
    1 -> displayH1 text
    2 -> displayH2 text
    3 -> displayH3 text
    4 -> displayH4 text
    5 -> displayH5 text
    6 -> displayH6 text
    _ -> displayH6 text  -- Fallback to H6 for any level > 6
  putStrLn ""
  setSGR [Reset]

readLine = do
  -- contents <- readFile()
  let contents = lines testCases
  parseLine contents


