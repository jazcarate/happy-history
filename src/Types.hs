module Types where

import           RIO
import           RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose    :: !Bool
  , optionsLogFile    :: !(Maybe FilePath)
  , optionsInitialCmd :: !Text
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

class HasInitialCmd env where
  initialCmdL :: Lens' env Text

instance HasInitialCmd App where
  initialCmdL =
    (lens appOptions (\x y -> x { appOptions = y }))
      . (lens optionsInitialCmd (\x y -> x { optionsInitialCmd = y }))
