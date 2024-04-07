module HTML.Common.Types where

import           Data.Text (Text)
import           Text.Megaparsec
import           Data.Void
import qualified Data.Map.Strict as Map

type Tag = Text
type Content = Text

type Parser = Parsec Void Text
type Attributes = Map.Map Text Text

data HTMLElement = HTMLElement Tag  Attributes [HTMLElement] | SelfClosingTag Tag Attributes | TextNode Content | CommentNode Text
  deriving (Show,Eq)
