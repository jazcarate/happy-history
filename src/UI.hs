module UI
  ( module UI
  , module System.Console.ANSI
  ) where

import           Prelude                        ( getChar )
import           RIO
import           RIO.ByteString                 ( putStr )
import qualified RIO.Char                      as C
import qualified RIO.Text                      as T
import           System.Console.ANSI -- TODO Check everything is Windows compatibles

data Widget = Widget
  { render :: IO ()
  }

data ControlKey = CKUnknown Text | CKEscape | CKSubmit | CKDel | CKUp | CKDown| CKRight | CKLeft | CKHome | CKEnd | CKTokenLeft | CKTokenRight | CKDelete

ctrl :: Text -> ControlKey
ctrl "\ESC"      = CKEscape
ctrl "\n"        = CKSubmit
ctrl "\ESC[D"    = CKLeft
ctrl "\ESC[C"    = CKRight
ctrl "\ESC[A"    = CKUp
ctrl "\ESC[B"    = CKDown
ctrl "\DEL"      = CKDel
ctrl "\ESC[1;5D" = CKTokenLeft
ctrl "\ESC[1;5C" = CKTokenRight
ctrl "\ESC[H"    = CKHome
ctrl "\ESC[F"    = CKEnd
ctrl "\DC2"      = CKUp
ctrl t           = CKUnknown t

data Event = Key Text | Control ControlKey

data Next a = Continue a | Finish a | Halt

type EventM a = IO a

halt :: EventM (Next a)
halt = pure Halt

finish :: a -> EventM (Next a)
finish = pure . Finish

continue :: a -> EventM (Next a)
continue = pure . Continue

type EventHandler state ev = state -> ev -> EventM (Next state)

data App state = App
  { appDraw        :: state -> Widget
  , appHandleEvent :: EventHandler state Event
  }

put :: Text -> IO ()
put = putStr . T.encodeUtf8

str :: Text -> Widget
str = Widget . put

app :: App State
app = App { appDraw = str, appHandleEvent = mainEventHandler }
-- TODO: Text -> Bytecode
data TextCursor = TextCursor -- TODO el cursor siempre tiene un extra " " (non empty listd)
  { txPrev :: Text
  , txCur  :: Text
  , txNext :: Text
  }

mkCursor :: Text -> TextCursor
mkCursor t = TextCursor mempty t mempty

next :: TextCursor -> TextCursor
next (TextCursor p c n) = let toMove = _ in TextCursor (p <> toMove)

prev :: TextCursor -> TextCursor
prev (TextCursor p c n) = TextCursor (max (p - 1) 0) v

explode :: TextCursor -> (Text, Char, Text)
explode (TextCursor p v) = (T.takeEnd Int Text)

type State = TextCursor

mainEventHandler :: EventHandler State Event
mainEventHandler state ev = case ev of
  (Key     word ) -> continue $ typeEH state word
  (Control chars) -> case chars of
    CKDel         -> continue $ T.dropEnd 1 state
    CKEscape      -> halt
    CKSubmit      -> finish state
    (CKUnknown c) -> continue $ toLit c

toLit :: Text -> Text
toLit t = T.concatMap (\c -> T.pack $ C.showLitChar c "") t

typeEH :: Text -> Text -> Text
typeEH = (<>)

getKey :: IO Text
getKey = T.reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) $ char `T.cons` chars

main :: MonadUnliftIO io => App state -> state -> io (Maybe state)
main app' = withUI . liftIO . loop
 where
  loop oldState = do
    restoreCursor
    clearFromCursorToScreenEnd
    render $ appDraw app' $ oldState
    key <- getKey
    let ev = event key
    res <- (appHandleEvent app') oldState ev
    case res of
      Continue newS   -> loop newS
      Finish   result -> pure $ Just result
      Halt            -> pure Nothing

event :: Text -> Event
event chars = if T.all C.isPrint chars then Key chars else Control (ctrl chars)

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
