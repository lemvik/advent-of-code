module Main where

import Data.List (concat, isInfixOf)
import Data.Text (Text)
import qualified Data.Text.IO as IO
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text (Parser)
import System.Environment (getArgs)

main :: IO ()
main = do
  [path] <- getArgs
  input <- readInput path
  print (countSupport supportsTLS input, countSupport supportsSSL input)

-- | Address part is either normal part or hypernet
data AddressPart = Normal String
                 | Hypernet String
                 deriving (Show, Eq)

-- | Ip v7 address struct
data Ipv7Address = Ipv7 { getAddresses :: [String]
                        , getHypernets :: [String] }
  deriving (Show, Eq)

-- | Parser that parses a part of address (string of letters)
addressPart :: Parser AddressPart
addressPart = Normal <$> many1 letter

-- | Parser that parses a hypernet part of the address
hypernetPart :: Parser AddressPart 
hypernetPart = do
  _ <- char '['
  res <- many1 letter
  _ <- char ']'
  return $ Hypernet res

-- | Parser that parses Ipv7 address
ipv7Address :: Parser Ipv7Address
ipv7Address = do
  parts <- many1 (addressPart <|> hypernetPart)
  return $ addressFromParts parts

-- | Constructs Ipv7address from given parts
addressFromParts :: [AddressPart] -> Ipv7Address
addressFromParts parts = go parts [] []
  where go [] normals hyper                  = Ipv7 (reverse normals) (reverse hyper)
        go (Normal s : rest) normals hyper   = go rest (s:normals) hyper
        go (Hypernet s : rest) normals hyper = go rest normals (s:hyper)

-- | Parses the address from the given text
parseAddress :: Text -> Either ParseError Ipv7Address
parseAddress = parse ipv7Address "text" 

-- | Reads input from given path returning list of IPv7 addresses.
readInput :: FilePath -> IO (Either ParseError [Ipv7Address])
readInput path = do
  contents <- T.lines <$> IO.readFile path
  return $ sequence $ parseAddress <$> contents 

-- | Returns true if given string has ABBA in it
hasABBA :: String -> Bool
hasABBA (a : rest@(b : c : d : _)) = (a == d && b == c && a /= b) || hasABBA rest
hasABBA _                          = False

-- | Returns true if given IPv7 address supports TLS
supportsTLS :: Ipv7Address -> Bool
supportsTLS (Ipv7 normals hypernets) = any hasABBA normals && not (any hasABBA hypernets)

-- | Returns true if given IPv7 address supports SSL
supportsSSL :: Ipv7Address -> Bool
supportsSSL (Ipv7 normals hypernets) = any hasMatch abas 
  where abas = concat $ collectAbas [] <$> normals 
        hasMatch [a,b,_] = any ([b,a,b] `isInfixOf`) hypernets
        hasMatch _       = False
        collectAbas :: [String] -> String -> [String]
        collectAbas acc (a : rest@(b : c : _)) | a == c    = collectAbas ([a, b, a]:acc) rest
                                               | otherwise = collectAbas acc rest
        collectAbas acc _ = acc

-- | Counts features support using given predicate
countSupport :: (Monad m) => (Ipv7Address -> Bool) -> m [Ipv7Address] -> m Int
countSupport p input = length . filter id . (p <$>) <$> input
