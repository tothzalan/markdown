module Lib
    ( Line
    , interpret
    , matchLine
    ) where

import Data.Char

data Line =
  Header Int String | Paragraph String | Unordered String |
  Ordered Int String | Blockquote String | Link String String |
  Image String String | HR | CodeBlock String
  deriving (Show, Eq)

interpret :: [String] -> IO ()
interpret = mapM_ (putStrLn . getTag) . matchLines
  where
    matchLines = reverse . foldl (\acc x -> (matchLine x acc):acc) []

isCodeBlock :: Line -> Bool
isCodeBlock (CodeBlock _) = True
isCodeBlock _             = False

matchLine :: String -> [Line] -> Line
matchLine ('#' : line)          _   = createHeader line
matchLine ('-' : ' ' : line)    _   = Unordered line
matchLine ('>' : ' ' : line)    _   = Blockquote line
matchLine ('`' : '`' : '`' : _) acc = createCodeBlock acc
matchLine ('-' : '-' : '-' : _) _   = HR
matchLine line acc
  | checkCodeBlock = CodeBlock line
  | isNumber (line !! 0) && (line !! 1) == '.' =
    createOList (digitToInt (line !! 0)) line
  | checkIfLink = createLink line
  | otherwise = Paragraph line
  where
    checkIfLink = elem '[' line && elem ']' line &&
      elem '(' line && elem ')' line
    checkCodeBlock = isCodeBlock (head acc) &&
      (head acc) /= (CodeBlock "</code></pre>")

-- Generate HTML from tags
getTag :: Line -> String
getTag (Header n l)   =
  ("<h" ++ (show n) ++ ">") ++ l ++ ("</h" ++ (show n) ++ ">")
getTag (Paragraph l)  = "<p>" ++ l ++ "</p>"
getTag (Unordered l)  = "<ul><li>" ++ l ++ "</li></ul>"
getTag (Ordered n l)  = "<ol start='" ++ show n ++ "'><li>" ++ l ++ "</li></ol>"
getTag (Blockquote l) = "<blockquote><p>" ++ l ++ "</p></blockquote>"
getTag (Link t l)     = "<a href='" ++ l ++ "'>" ++ t ++ "</a>"
getTag (Image t l)    = "<img src='" ++ l ++ "' alt='" ++ t ++ "'/>"
getTag HR             = "<hr/>"
getTag (CodeBlock l)  = l

-- Parse tags
createHeader :: String -> Line
createHeader line
  | (hSize line) < 7 = (Header (hSize line) (drop (hSize line) line))
  | otherwise        = (Header 6 (drop 6 line))
    where
      hSize = (+1) . length . takeWhile (=='#')

createOList :: Int -> String -> Line
createOList n = Ordered n . drop 3

createLink :: String -> Line
createLink line = (isImage line) (getText line) (getLink line)
  where
    isImage ('!' : _) = Image
    isImage _         = Link
    getText = tail . takeWhile (/=']') . dropWhile (/='[')
    getLink = tail . takeWhile (/=')') . dropWhile (/='(')

createCodeBlock :: [Line] -> Line
createCodeBlock acc
  | count "<code><pre>" == count "</code></pre>" =
    CodeBlock "<code><pre>"
  | otherwise =
    CodeBlock "</code></pre>"
  where
    count s = length $ filter (==(CodeBlock s)) acc
