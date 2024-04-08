{-# LANGUAGE OverloadedStrings #-}
module HTML.Core where

import           HTML.Common.Types
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map.Strict as Map

sc :: Parser ()
sc = L.space space1 empty empty

tagName :: Parser Text
tagName = T.pack <$> (many alphaNumChar)

pOpenTag :: Parser (Text,Map.Map Text Text)
pOpenTag = between (char '<') (char '>') ((,) <$> tagName <*> pAttributes)

pCloseTag :: Parser Text
pCloseTag = T.pack <$> between (string "</") (char '>') (many alphaNumChar)

pText :: Parser HTMLElement
pText = TextNode.(T.pack) <$> some (noneOf ['<','>'])

pContents :: Parser [HTMLElement]
pContents = many (try pHtmlElement <|> try selfClosingTag <|> try pComment <|> pText)

pComment :: Parser HTMLElement
pComment = CommentNode.(T.pack) <$> between (string "<!--") (string "-->") 
  (many (alphaNumChar <|> char ' '))

selfClosingTag :: Parser HTMLElement
selfClosingTag = do
  _ <- char '<'
  name <- tagName
  attrs <- sc *> pAttributes
  _ <- string "/>"
  _ <- many (char '\n' <|> char ' ' <|> char '\t')
  return $ SelfClosingTag name attrs

pAttribute :: Parser (Text,Text)
pAttribute = do
  key   <- some (alphaNumChar <|> char '-')
  _     <- try $ char '='
  value <- between (char '"') (char '"') (many (alphaNumChar
    <|> char '-'
    <|> char '.'
    <|> char '@'
    <|> char '.'
    <|> char ' '
    <|> char '#'
    <|> char '/'
    <|> char ':'
    <|> char '+'
    <|> char '='
    <|> char ','
    ))
  pure (T.pack key,T.pack value)

pAttributes :: Parser (Map.Map Text Text)
pAttributes = Map.fromList <$> many (sc *> pAttribute <* sc)

pHtmlElement :: Parser HTMLElement
pHtmlElement = do
  (name,attr)  <- pOpenTag
  _ <- many (char '\n' <|> char ' ' <|> char '\t')
  inner <- pContents
  _ <- many (char '\n' <|> char ' ' <|> char '\t')
  closeName     <- pCloseTag
  _ <- many (char '\n' <|> char ' ' <|> char '\t')
  (if name == closeName then return $ HTMLElement name attr inner else failure Nothing mempty)

htmlDocument :: Parser [HTMLElement]
htmlDocument = between sc (many space1 <* eof) (many pHtmlElement)

toHTMLElement :: Text -> Either Text [HTMLElement]
toHTMLElement content = case parse htmlDocument "" content of
  Left _ -> Left "Parsing Failed :("
  Right r -> Right r
