module Main
  ( main
  ) where

import           Options.Applicative.Simple
import           RIO
import           RIO.Process
import qualified RIO.Text
import           Run
import           System.IO                      ( openFile )
import           Types
import           Version


-- TODO: search command, config? install?
main :: IO ()
main = do
  (options, ()) <- simpleOptions
    version
    "Happy History"
    "Navigate though history and re-use commands with ease "
    (   Options
    <$> (optional $ strOption
          (long "logfile" <> help "Where to log" <> metavar "FILE" <> action
            "file"
          )
        )
    <*> (RIO.Text.concat <$> some (argument str (metavar "SEARCH TERMS...")))
    )
    empty

  logHandle <- case optionsLogFile options of
    Just path -> openFile path WriteMode
    Nothing   -> pure stderr -- TODO: Handle better

  lo <- setLogMinLevel LevelDebug <$> logOptionsHandle logHandle False --TODO Not logging?
  pc <- mkDefaultProcessContext

  withLogFunc lo $ \lf ->
    let app = App { appLogFunc        = lf
                  , appProcessContext = pc
                  , appOptions        = options
                  }
    in  runRIO app run
