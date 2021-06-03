module UI
  ( module UI
  , module System.Console.ANSI
  ) where

import           Prelude                        ( getChar )
import           RIO
import           RIO.ByteString                 ( putStr )
import qualified RIO.Char                      as C
import qualified RIO.Text                      as T
import           System.Console.ANSI

data Widget = Widget
  { render :: IO ()
  }

data Event = Key Text

data Next a = Continue a | Halt (Maybe a)

type EventM a = ReaderT a IO (Next a)

newtype EventHandler state = EventHandler { eventHandlerRun :: Event -> EventM state }


instance Semigroup s => Semigroup (EventHandler s) where
  (EventHandler ra) <> (EventHandler rb) = EventHandler $ \ev ->
    ra ev
      >>= (\newS -> case newS of
            Halt     _ -> pure newS
            Continue _ -> rb ev
          )

data App state = App
  { appDraw        :: state -> Widget
  , appHandleEvent :: EventHandler state
  }

put :: Text -> IO ()
put = putStr . T.encodeUtf8

str :: Text -> Widget
str = Widget . put

app :: App Text
app = App { appDraw = str, appHandleEvent = terminationEH <> typeEH }

terminationEH :: EventHandler state
terminationEH = EventHandler $ \(Key t) -> case t of
  "\ESC" -> pure $ Halt Nothing
  "\n"   -> Halt . Just <$> ask
  _      -> Continue <$> ask

typeEH :: EventHandler Text
typeEH = EventHandler $ \(Key t) -> if T.all C.isPrint t
  then (\s -> Continue (s <> t)) <$> ask
  else Continue <$> ask

getKey :: IO Text
getKey = T.reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) $ char `T.cons` chars

runEventHandler :: EventHandler s -> Event -> s -> IO (Next s)
runEventHandler (EventHandler r) ev = runReaderT (r ev)

main :: MonadUnliftIO io => App state -> state -> io (Maybe state)
main (App draw handler) = withUI . liftIO . loop
 where
  loop oldState = do
    restoreCursor -- TODO smarter re-render
    clearFromCursorToScreenEnd
    render $ draw oldState
    key <- getKey
    res <- runEventHandler handler (Key key) oldState
    case res of
      Continue newS   -> loop newS
      Halt     result -> pure result

init :: MonadUnliftIO m => m ()
init = liftIO $ do
  cursorUp 1 -- TODO: What about multi-line prompts? env config?
  saveCursor

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  clearFromCursorToScreenEnd
  setSGR [SetColor Foreground Vivid Red]

close :: MonadUnliftIO m => m ()
close = liftIO $ do
  setSGR [Reset]
  restoreCursor
  clearFromCursorToScreenEnd


withUI :: MonadUnliftIO m => m a -> m a
withUI = bracket_ init close

test :: IO ()
test = do
  res <- withRunInIO $ \runInIO -> runInIO $ main app "foo"
  case res of
    Nothing -> put "Aborted\n"
    Just r  -> put $ "Result: " <> r <> "\n"
