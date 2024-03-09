module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Tuple

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving Show

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ (fmap . fmap) f . p

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)
  Parser p1 <*> Parser p2 = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    return (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \input -> p1 input <|> p2 input

ws :: Parser String
ws = spanP isSpace

charP :: Char -> Parser Char
charP c = Parser parse where
  parse (x:xs) | x == c = pure (xs, x)
  parse _ = empty

sepP :: Char -> Parser Char
sepP c = ws *> charP c <* ws

stringP :: String -> Parser String
stringP = traverse charP

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

spanP :: (Char -> Bool) -> Parser String
spanP pred = Parser $ pure . swap . span pred

notNull :: Parser String -> Parser String
notNull (Parser p) = Parser parse where
  parse input = do
    parsed <- p input
    guard $ not . null . snd $ parsed
    return parsed

sepBy :: Char -> Parser a -> Parser [a]
sepBy sep elem = (:) <$> elem <*> many (sepP sep *> elem) <|> pure []

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = parseTrue <|> parseFalse where
  parseTrue = JsonBool True <$ stringP "true"
  parseFalse = JsonBool False <$ stringP "false"

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> notNull (spanP isDigit)

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (sepP '[' *> sepBy ',' jsonValue <* sepP ']')

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (sepP '{' *> sepBy ',' pair <* sepP '}') where
  pair = (,) <$> (stringLiteral <* sepP ':') <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = foldr1 (<|>) parsers where
  parsers = [jsonNull, jsonBool, jsonNumber, jsonString, jsonArray, jsonObject]

main :: IO ()
main = interact $ show . runParser jsonValue
