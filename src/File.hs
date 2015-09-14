module File where

{- Parse the input Files for the names list, the interwiki link database
and the page count statistics -}

import Prelude hiding (takeWhile)
import Data.List
import Control.Comonad
import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString

type Outlinks = (Int,[Int])
type PageCount = (Int,Int)

idPageCountList :: [(B.ByteString,Int)] -> [B.ByteString] -> [PageCount]
idPageCountList [] ns = []
idPageCountList (p:ps) ns = case (elemIndex (fst p) ns) of
                        Just a -> (a+1,snd p) : (idPageCountList ps ns)
                        Nothing -> idPageCountList ps ns

namesParser' :: C.Parser B.ByteString
namesParser' = takeTill C.isEndOfLine

namesParser :: C.Parser [B.ByteString]
namesParser = sepBy namesParser' C.endOfLine

pageCountParser' :: C.Parser (B.ByteString,Int)
pageCountParser' = do
    C.char 'e'; C.char 'n'; C.char ' '
    articleName <- C.takeTill (== ' ')
    C.char ' '
    n <- C.decimal
    C.char ' '
    C.decimal
    return (articleName,n)

pageCountParser :: C.Parser [(B.ByteString,Int)]
pageCountParser = sepBy pageCountParser' C.endOfLine

linksParser' :: Parser Outlinks
linksParser' = do
    articleID <- C.decimal
    C.char ':'
    C.char ' '
    links <- sepBy C.decimal $ C.char ' '
    return (articleID,links)

linksParser :: Parser [Outlinks]
linksParser = sepBy linksParser' C.endOfLine

{------------------- TEST --------------------------

main = do
    linksFile <- B.readFile "links"
    countsFile <- B.readFile "counts"
    namesFile <- B.readFile "names"
    print (parseOnly linksParser linksFile)
    print (parseOnly pageCountParser countsFile)
    print (parseOnly namesParser namesFile)
    print "Have a nice day"
--------------------------------------------------}
