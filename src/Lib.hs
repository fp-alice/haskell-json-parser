module Lib where

import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Text.Parsec.Combinator
import           Text.ParserCombinators.Parsec

data Json
     = Object (Map String Json)
     | Int Int
     | String String
     | Bool Bool
     | Null
     | Array [Json] deriving (Show)


readJson :: IO String
readJson = readFile "test.json"

stringParser :: Parser Json
stringParser = do
  _ <- char '\"'
  s <- manyTill anyChar (char '\"')
  return $ String s

boolParser :: Parser Json
boolParser = do
  b <- string "true" <|> string "false"
  return $ Bool (b == "true")

intParser :: Parser Json
intParser = do
  n <- many1 digit
  return $ Int (read n)

nullParser :: Parser Json
nullParser = do
  _ <- string "null"
  return Null

arrayParser :: Parser Json
arrayParser = do
  _ <- char '['
  a <- sepBy jsonParser (spaces *> string "," <* spaces)
  _ <- char ']'
  return $ Array a

pairParser :: Parser (String, Json)
pairParser = do
  (String s) <- stringParser
  _ <- char ':' <* spaces
  j <- jsonParser <* optional (char ',') <* optional spaces
  return (s, j)

objectParser :: Parser Json
objectParser = do
  _ <- char '{' <* spaces
  o <- sepBy pairParser spaces
  _ <- char '}'
  return $ Object (Map.fromList o)

jsonParser :: Parser Json
jsonParser = stringParser <|> intParser <|> nullParser <|> arrayParser <|> objectParser <|> boolParser
