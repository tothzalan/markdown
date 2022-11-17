import System.IO
import System.Environment
import Data.List
import Data.Char
import Control.Monad

data Line =
  Header Int String | Paragraph String | Unordered String |
  Ordered Int String | Blockquote String | Link String String |
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

isCodeBlock (CodeBlock _) = True
isCodeBlock _             = False

matchLine :: String -> [Line] -> Line
matchLine ('#' : line) _ = createHeader line
matchLine ('-' : ' ' : line) _ = createUList line
matchLine ('>' : ' ' : line) _ = createBlockquote line
matchLine ('`' : '`' : '`' : line) acc = createCodeBlock line acc
matchLine ('-' : '-' : '-' : line) _ = HR
matchLine line acc
  | isCodeBlock (head acc) &&
    (head acc) /= (CodeBlock "</code></pre>") = CodeBlock line
  | isNumber (line !! 0) && (line !! 1) == '.' = createOList (digitToInt (line !! 0)) line
  | checkIfLink line = createLink line
  | otherwise = createParagraph line
  where
    checkIfLink line = elem '[' line && elem ']' line
      && elem '(' line && elem ')' line

-- Generate HTML from tags
getTag :: Line -> String
getTag (Header n l)   =
  ("<h" ++ [intToDigit n] ++ ">") ++ l ++ ("</h" ++ [intToDigit n] ++ ">")
getTag (Paragraph l)  = "<p>" ++ l ++ "</p>"
getTag (Unordered l)  = "<ul><li>" ++ l ++ "</li></ul>"
getTag (Ordered n l)    = "<ol start='" ++ show n ++ "'><li>" ++ l ++ "</li></ol>"
getTag (Blockquote l) = "<blockquote><p>" ++ l ++ "</p></blockquote>"
getTag (Link t l)     = "<a href='" ++ l ++ "'>" ++ t ++ "</a>"
getTag (Image t l)    = "<img src='" ++ l ++ "' alt='" ++ t ++ "'/>"
getTag HR             = "<hr/>"
getTag (CodeBlock l)     = l

-- Parse tags
createHeader :: String -> Line
createHeader line
  | (hSize line) < 7 = (Header (hSize line) (drop (hSize line) line))
  | otherwise        = (Header 6 (drop 6 line))
    where
      hSize = (+1) . length . takeWhile (=='#')

createParagraph :: String -> Line
createParagraph line = (Paragraph line)

createUList :: String -> Line
createUList line = (Unordered $ line)

createOList :: Int -> String -> Line
createOList n line = (Ordered n $ drop 2 line)

createBlockquote :: String -> Line
createBlockquote line = (Blockquote $ line)

createLink :: String -> Line
createLink line = ((isImage line) (getText line) (getLink line))
  where
    isImage line
      | head line == '!' = Image
      | otherwise        = Link
    getText = tail . takeWhile (/=']') . dropWhile (/='[')
    getLink = tail . takeWhile (/=')') . dropWhile (/='(')

createCodeBlock :: String -> [Line] -> Line
createCodeBlock line acc
  | nStart acc == nEnd acc = CodeBlock "<code><pre>"
  | otherwise              = CodeBlock "</code></pre>"
  where
    nStart = length . filter (==(CodeBlock "<code><pre>"))
    nEnd   = length . filter (==(CodeBlock "</code></pre>"))
