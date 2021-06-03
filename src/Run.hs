module Run
  ( run
  ) where

import           Import
import qualified RIO.Text                      as T
import qualified RIO.Vector.Boxed              as VB
import qualified UI

readHistory :: MonadIO m => m (Vector Text)
readHistory = VB.fromList <$> T.lines <$> readFileUtf8 "/tmp/.hh_history"

run :: RIO Import.App ()
run = do
  history <- readHistory

  logInfo
    $  "History had "
    <> (fromString $ show $ VB.length history)
    <> " lines"

  cmd    <- asks (^. initialArgsL)
  result <- UI.main UI.app cmd

  writeFileUtf8 "/tmp/.hh_last_command" "foo" -- TODO write result
