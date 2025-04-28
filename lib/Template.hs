{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Template where

import qualified Formatting as F
import Text.Mustache (Template, ToMustache (..), object)
import Text.Mustache.Types ((~>))

data TemplateData = TemplateData
  { items :: [(String, String)]
  }

instance Semigroup TemplateData where
  (TemplateData items0) <> (TemplateData items1) =
    TemplateData $ items0 ++ items1

instance Monoid TemplateData where
  mempty = TemplateData []

getDefaultTemplateData :: IO TemplateData
getDefaultTemplateData = do
  date <- F.getFormattedDate
  return $ TemplateData [("date", date)]

data DefaultTemplateData = DefaultTemplateData
  { title :: String,
    date :: String
  }

instance ToMustache DefaultTemplateData where
  toMustache DefaultTemplateData {..} =
    object
      [ "title" ~> title,
        "date" ~> date
      ]
