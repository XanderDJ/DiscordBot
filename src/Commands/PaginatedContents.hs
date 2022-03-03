module Commands.PaginatedContents where

import Data.Text

type FieldName = Text

type FieldContent = Text

data PaginatedContents = PaginatedContents
  { paginatedTitle :: Text,
    paginatedText :: Text,
    paginatedFieldsMap :: [(FieldName, [FieldContent])]
  } deriving (Show, Eq)