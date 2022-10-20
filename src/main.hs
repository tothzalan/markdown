import System.IO
import System.Environment
import Data.List
import Data.Char
import Control.Monad

data Line =
  Header Int String | Paragraph String | Unordered String |
  Ordered String | Blockquote String | Link String String |
  Image String String | HR
  deriving (Show, Eq)

main = do
  args <- getArgs
  if length args /= 1
    then error "Wrong number of arguments!"
    --else putStrLn ("Interpreting: " ++ head args)
    else putStr ""
  fileString <- do readFile (head args)
  let file = lines fileString
  interpret file

interpret :: [String] -> IO ()
interpret lines = mapM_  (\x -> putStrLn (getTag x)) (reverse (magic lines []))
  where
    magic []        acc = acc
    magic (line:ls) acc = magic ls ((matchLine line):acc)

matchLine :: String -> Line
matchLine line
  | isPrefixOf "#" line   = createHeader line
  | isPrefixOf "---" line = HR
  | isPrefixOf "-" line   = createUList line
  | isPrefixOf ">" line   = createBlockquote line
  | isNumber (line !! 0) && (line !! 1) == '.' = createOList line
  | otherwise = createParagraph line


-- Generate HTML from tags
getTag :: Line -> String
getTag (Header n l)   =
  ("<h" ++ [intToDigit n] ++ ">") ++ l ++ ("</h" ++ [intToDigit n] ++ ">")
getTag (Paragraph l)  = "<p>" ++ l ++ "</p>"
getTag (Unordered l)  = "<li>" ++ l ++ "</li>"
getTag (Ordered l)    = "<li>" ++ l ++ "</li>"
getTag (Blockquote l) = "<blockquote><p>" ++ l ++ "</p></blockqoute>"
getTag HR             = "<hr/>"

-- Parse tags
createHeader :: String -> Line
createHeader line
  | (hSize line 0) < 7 = (Header (hSize line 0) (drop (hSize line 0) line))
  | otherwise = (Header 6 (drop 6 line))
    where
      hSize []     acc = acc
      hSize (l:ls) acc
        | l == '#'  = hSize ls (acc+1)
        | otherwise = acc

createParagraph :: String -> Line
createParagraph line = (Paragraph line)

createUList :: String -> Line
createUList line = (Unordered (drop 1 line))

createOList :: String -> Line
createOList line = (Ordered (drop 2 line))

createBlockquote :: String -> Line
createBlockquote line = (Blockquote (drop 1 line))
