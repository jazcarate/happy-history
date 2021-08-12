{-# LANGUAGE ScopedTypeVariables #-}

module History where

import qualified Data.Text.Read                as TR
import           RIO
import qualified RIO.Text                      as T

data PastCommand = PastCommand
  { command :: Text
  }

pastCommand :: Text -> Maybe PastCommand
pastCommand line = case TR.decimal $ T.stripStart line of
  Right (_ :: Integer, rest) -> PastCommand <$> T.stripPrefix "  " rest
  _                          -> Nothing
