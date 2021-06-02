{-# LANGUAGE TemplateHaskell #-}

module Version
  ( version
  ) where

import qualified Paths_happy_history

import           Options.Applicative.Simple     ( simpleVersion )
import           RIO

version :: String
version = $(simpleVersion Paths_happy_history.version)
