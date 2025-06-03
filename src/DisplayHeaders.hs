-- header display functions that decrease sizes as the header level increases, similar to how HTML/markdown headers work
module DisplayHeaders (displayH1
                       , displayH2
                       , displayH3
                       , displayH4
                       , displayH5
                       , displayH6) where
import System.Console.ANSI
import Data.Char (toUpper)

-- Header 1: Largest - Double-height effect with box
displayH1 :: String -> IO ()
displayH1 text = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
  let upperText = map toUpper text
  let padding = 4
  let totalWidth = length upperText + padding * 2
  putStrLn $ "╔" ++ replicate totalWidth '═' ++ "╗"
  putStrLn $ "║" ++ replicate totalWidth ' ' ++ "║"
  putStrLn $ "║" ++ replicate padding ' ' ++ upperText ++ replicate padding ' ' ++ "║"
  putStrLn $ "║" ++ replicate totalWidth ' ' ++ "║"
  putStrLn $ "╚" ++ replicate totalWidth '═' ++ "╝"

-- Header 2: Large - Single box with uppercase
displayH2 :: String -> IO ()
displayH2 text = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
  let upperText = map toUpper text
  putStrLn $ "┌" ++ replicate (length upperText + 4) '─' ++ "┐"
  putStrLn $ "│  " ++ upperText ++ "  │"
  putStrLn $ "└" ++ replicate (length upperText + 4) '─' ++ "┘"


-- Header 3: Medium-Large - Bold underlined
displayH3 :: String -> IO ()
displayH3 text = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]
  putStrLn $ "  " ++ text
  putStrLn $ "  " ++ replicate (length text) '='

-- Header 4: Medium - Simple underlined
displayH4 :: String -> IO ()
displayH4 text = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
  putStrLn $ "   " ++ text
  putStrLn $ "   " ++ replicate (length text) '-'

-- Header 5: Small - Indented with arrows
displayH5 :: String -> IO ()
displayH5 text = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
  putStrLn $ "    > " ++ text

-- Header 6: Smallest - Simple indented bullet
displayH6 :: String -> IO ()
displayH6 text = do
  setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Magenta]
  putStrLn $ "     • " ++ text