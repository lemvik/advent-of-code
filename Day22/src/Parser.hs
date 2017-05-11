module Parser where

import Definitions
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text

-- | Parses size (dropping T)
parseSize :: Parser Size
parseSize = do
  s <- read <$> many1 digit
  _ <- char 'T'
  return s

-- | Parses disk entry
parseDisk :: Parser Disk
parseDisk = do
  _ <- string "/dev/grid/node-x"
  x <- read <$> many1 digit
  _ <- string "-y"
  y <- read <$> many1 digit
  spaces
  s <- parseSize
  spaces
  u <- parseSize
  spaces
  a <- parseSize
  return $ Disk (x, y) s u a

-- | Reads enttries from lines
readEntries :: [Text] -> Either ParseError [Disk]
readEntries lns = sequence $ parse parseDisk "lines" <$> lns
