module Types where

import           RIO
import           RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose     :: !Bool
  , optionsLogFile     :: !(Maybe FilePath)
  , optionsInitialArgs :: !Text
  }

data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions        :: !Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })

class HasInitialArgs env where
  initialArgsL :: Lens' env Text
instance HasInitialArgs App where
  initialArgsL =
    (lens appOptions (\x y -> x { appOptions = y }))
      . (lens optionsInitialArgs (\x y -> x { optionsInitialArgs = y }))
