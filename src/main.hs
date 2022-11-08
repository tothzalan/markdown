import System.IO
import System.Environment
import Data.List
import Data.Char
import Control.Monad

data Line =
  Header Int String | Paragraph String | Unordered String |
  Ordered String | Blockquote String | Link String String |
  Image String String | HR | CodeBlock String
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
interpret lines = mapM_  (putStrLn . getTag) (magic lines)
  where
    magic :: [String] -> [Line]
    magic = reverse . foldl (\acc x -> (matchLine x acc):acc) []
    


matchLine :: String -> [Line] -> Line
matchLine line acc
  | isPrefixOf "#" line   = createHeader line
  | isPrefixOf "---" line = HR
  | isPrefixOf "-" line   = createUList line
  | isPrefixOf ">" line   = createBlockquote line
  | isNumber (line !! 0) && (line !! 1) == '.' = createOList line
  | checkIfLink line = createLink line
  | isCodeBlock (head acc) = createCodeBlock line
  | isPrefixOf "```" line  = createCodeBlock line
  | otherwise = createParagraph line
  where
    checkIfLink line = elem '[' line && elem ']' line
      && elem '(' line && elem ')' line
    isCodeBlock (CodeBlock _) = True
    isCodeBlock _             = False

-- Generate HTML from tags
getTag :: Line -> String
getTag (Header n l)   =
  ("<h" ++ [intToDigit n] ++ ">") ++ l ++ ("</h" ++ [intToDigit n] ++ ">")
getTag (Paragraph l)  = "<p>" ++ l ++ "</p>"
getTag (Unordered l)  = "<li>" ++ l ++ "</li>"
getTag (Ordered l)    = "<li>" ++ l ++ "</li>"
getTag (Blockquote l) = "<blockquote><p>" ++ l ++ "</p></blockquote>"
getTag (Link t l)     = "<a href='" ++ l ++ "'>" ++ t ++ "</a>"
getTag (Image t l)    = "<img src='" ++ l ++ "' alt='" ++ t ++ "'/>"
getTag HR             = "<hr/>"
getTag (CodeBlock l)     = l

-- Parse tags
createHeader :: String -> Line
createHeader line
  | (hSize line) < 7 = (Header (hSize line) (drop (hSize line) line))
  | otherwise          = (Header 6 (drop 6 line))
    where
      hSize = length . takeWhile (=='#')

createParagraph :: String -> Line
createParagraph line = (Paragraph line)

createUList :: String -> Line
createUList line = (Unordered $ drop 1 line)

createOList :: String -> Line
createOList line = (Ordered $ drop 2 line)

createBlockquote :: String -> Line
createBlockquote line = (Blockquote $ drop 1 line)

createLink :: String -> Line
createLink line = ((isImage line) (getText line) (getLink line))
  where
    isImage line
      | head line == '!' = Image
      | otherwise        = Link
    getText = tail . takeWhile (/=']') . dropWhile (/='[')
    getLink = tail . takeWhile (/=')') . dropWhile (/='(')

createCodeBlock :: String -> Line
createCodeBlock line
  | line == "```"         = (CodeBlock "</code></pre>")
  | isPrefixOf "```" line = (CodeBlock "<code><pre>")
  | otherwise             = (CodeBlock line)
