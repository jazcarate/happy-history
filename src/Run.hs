module Run
  ( run
  ) where

import qualified History                       as H
import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V
import           Types
import qualified UI


readHistory :: MonadIO m => m (Vector H.PastCommand)
readHistory =
  V.mapMaybe H.pastCommand <$> V.fromList <$> T.lines <$> readFileUtf8
    "/tmp/.hh_history"

run :: RIO App ()
run = do
  history <- readHistory

  logInfo $ "History had " <> (fromString $ show $ V.length history) <> " lines"

  cmd <- asks (^. initialCmdL)
  let initialState = UI.mkState cmd history
  result <- UI.main initialState
  case result of
    Just toExecute -> do
      writeFileUtf8 "/tmp/.hh_last_command" toExecute
      exitSuccess
    Nothing -> exitFailure
