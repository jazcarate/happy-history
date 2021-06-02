module Run
  ( run
  ) where

import           Data.Char                      ( showLitChar )
import           Import
import           Prelude                        ( getChar )
import qualified RIO.ByteString                as B
import           RIO.Char                       ( isPrint )
import qualified RIO.Text                      as T
import qualified RIO.Vector.Boxed              as VB
import           System.Console.ANSI

readHistory :: MonadIO m => m (Vector Text)
readHistory = VB.fromList <$> T.lines <$> readFileUtf8 "/tmp/.hh_history"

run :: RIO Import.App ()
run = do
  env     <- ask
  history <- readHistory

  logInfo
    $  "History had "
    <> (fromString $ show $ VB.length history)
    <> " lines"
    -- Init
  liftIO $ cursorUp 1 -- TODO: What about multi-line prompts? env config?
  liftIO $ saveCursor
  liftIO $ clearFromCursorToScreenEnd
  liftIO $ setSGR
    [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  cmd <- loop $ view initialArgsL env
  writeFileUtf8 "/tmp/.hh_last_command" cmd

  -- Cleanup
  liftIO $ setSGR [Reset]
  hFlush stdout
 where
  loop
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
    => Text
    -> m Text
  loop s = do
    liftIO $ restoreCursor
    liftIO $ clearFromCursorToScreenEnd
    B.putStr $ T.encodeUtf8 s
    key <- liftIO $ getKey
    case key of
      "\DEL" -> loop $ T.dropEnd 1 s
      "\ESC" -> pure s
      "\n"   -> pure s
      _      -> if T.all isPrint key
        then loop (s <> key)
        else do
          logWarn "Not all printable D:"
          pure s

-- Copy keys from: https://github.com/jtdaugherty/vty/blob/master/src/Graphics/Vty/Input/Terminfo/ANSIVT.hs

getKey :: IO Text
getKey = T.reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) $ char `T.cons` chars
