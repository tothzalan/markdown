import System.IO
import System.Environment
import Data.List
import Data.Char
import Control.Monad

--data Line = Ordered String | Unordered String | Code String
--  deriving (Show, Eq)

main = do
  args <- getArgs
  if length args /= 1
    then error "Wrong number of arguments!"
    --else putStrLn ("Interpreting: " ++ head args)
    else putStr ""
  fileString <- do readFile (head args)
  let file = lines fileString
  mapM_ interpret file

interpret :: String -> IO ()
interpret line
  | isPrefixOf "#" line = putStrLn (createHeader line (headerSize line))
  | isPrefixOf "---" line = putStrLn "<hr/>"
  | isPrefixOf "-" line = putStrLn (createUList line)
  | isPrefixOf ">" line = putStrLn (createBlockQuote line)
  | otherwise = putStrLn ("<p>" ++ line ++ "</p>")

tagStart :: String -> String
tagStart tag = "<" ++ tag ++ ">"

tagEnd :: String -> String
tagEnd tag = "</" ++ tag ++ ">"

headerSize :: String -> Int
headerSize line = count line 0 where
  count []     acc = acc
  count (l:ls) acc
    | l == '#'  = count ls (acc+1)
    | otherwise = count ls acc

createHeader :: String -> Int -> String
createHeader line n
  | n < 7 = (tagStart' "h" n) ++ drop n line ++ (tagEnd' "h" n)
  | otherwise = (tagStart "h6") ++ drop 6 line ++ (tagEnd "h6") where
  tagStart' :: String -> Int -> String
  tagStart' tag num = tagStart $ "h" ++ (intToDigit num):[]
  tagEnd' :: String -> Int -> String
  tagEnd' tag num = tagEnd $ "h" ++ (intToDigit num):[]

createUList :: String -> String
createUList line = "<li>" ++ drop 1 line ++ "</li>"

createBlockQuote :: String -> String
createBlockQuote line = (tagStart "blockquote") ++
  (tagStart "p") ++ drop 1 line ++
  (tagEnd "p") ++ (tagEnd "blockquote")
