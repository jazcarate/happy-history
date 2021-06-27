module UI.Fwk
  ( module UI.Fwk
  , module System.Console.ANSI
  ) where


import           Cursor.Text
import           Prelude                        ( getChar )
import           RIO
import           RIO.ByteString                 ( putStr )
import qualified RIO.Char                      as C
import qualified RIO.Text                      as T
import           System.Console.ANSI
import qualified Types -- TODO Check everything is Windows compatibles

data Widget = Widget
  { widgetRender :: IO ()
  }

instance Semigroup Widget where
  (Widget r1) <> (Widget r2) = Widget $ r1 >> r2

instance Monoid Widget where
  mempty = Widget mempty

data ControlKey = CKUnknown Text | CKEscape | CKSubmit | CKDel | CKSupr | CKUp | CKDown| CKRight | CKLeft | CKHome | CKEnd

ctrl :: Text -> ControlKey
ctrl "\ESC"   = CKEscape
ctrl "\n"     = CKSubmit
ctrl "\ESC[D" = CKLeft
ctrl "\ESC[C" = CKRight
ctrl "\ESC[A" = CKUp
ctrl "\ESC[B" = CKDown
ctrl "\DEL"   = CKDel
-- ctrl "\SUPR" = CKSupr
-- ctrl "\ESC[1;5D" = CKTokenLeft
-- ctrl "\ESC[1;5C" = CKTokenRight
ctrl "\ESC[H" = CKHome
ctrl "\ESC[F" = CKEnd
ctrl "\DC2"   = CKUp
ctrl t        = CKUnknown t

data Event = Key Text | Control ControlKey

data Next a = Continue a | Finish a | Halt

instance Functor Next where
  fmap f (Continue a) = Continue (f a)
  fmap f (Finish   a) = Finish (f a)
  fmap _ Halt         = Halt

type EventM a = RIO Types.App a

halt :: EventM (Next a)
halt = pure Halt

finish :: a -> EventM (Next a)
finish = pure . Finish

continue :: a -> EventM (Next a)
continue = pure . Continue

type EventHandler state ev = state -> ev -> EventM (Next state)

andThen
  :: EventHandler state ev -> EventHandler state ev -> EventHandler state ev
andThen a b = \state ev -> do
  s1 <- a state ev
  case s1 of
    Continue s1' -> b s1' ev
    _            -> pure s1

data App state = App
  { appDraw        :: state -> Widget
  , appHandleEvent :: EventHandler state Event
  }

put :: Text -> IO ()
put = putStr . T.encodeUtf8

type Style = [SGR]

withStyle :: Style -> IO c -> IO c
withStyle style = bracket_ (setSGR style) (setSGR [Reset])

noStyle :: Style
noStyle = mempty

str :: [SGR] -> Text -> Widget
str style t = Widget $ withStyle style (put t)

editorRender :: Editor -> Widget
editorRender (Editor e) =
  str noStyle (textCursorBefore e)
    <> str [SetColor Foreground Vivid White, SetColor Background Dull Blue]
           (textCursorCurrent e)
    <> str noStyle (textCursorAfter e)


newtype Editor = Editor
  { editorContent :: TextCursor
  }

mkEditor :: Text -> Editor
mkEditor = Editor . mkTextCursor

editorContentL :: Lens' Editor TextCursor
editorContentL = lens editorContent (\x y -> x { editorContent = y })

editorGetText :: Editor -> Text
editorGetText = getText . editorContent

editorEH :: EventHandler Editor Event
editorEH editor ev = case ev of
  (Key     word ) -> continue $ editor & editorContentL %~ (append word)
  (Control chars) -> case chars of
    CKDel    -> continue $ editor & editorContentL %~ delete
    CKEscape -> halt
    CKSubmit -> finish editor
    CKLeft   -> continue $ editor & editorContentL %~ prev
    CKRight  -> continue $ editor & editorContentL %~ next
    CKHome   -> continue $ editor & editorContentL %~ home
    CKEnd    -> continue $ editor & editorContentL %~ end
    _        -> continue editor

toLit :: Text -> Text
toLit t = T.concatMap (\c -> T.pack $ C.showLitChar c "") t

getKey :: IO Text
getKey = T.reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) $ char `T.cons` chars

start :: App state -> state -> RIO Types.App (Maybe state)
start app' = withUI . loop
 where
  loop oldState = do
    liftIO restoreCursor
    liftIO clearFromCursorToScreenEnd
    liftIO $ widgetRender $ appDraw app' $ oldState
    key <- liftIO getKey
    logDebug $ "Keypress: " <> (display $ toLit key)
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
  hideCursor
  cursorUp 1 -- TODO: What about multi-line prompts? env config?
  saveCursor

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  clearFromCursorToScreenEnd
  setSGR [SetColor Foreground Vivid Red]

close :: MonadUnliftIO m => m ()
close = liftIO $ do
  setSGR [Reset]
  showCursor
  restoreCursor
  clearFromCursorToScreenEnd


withUI :: MonadUnliftIO m => m a -> m a
withUI = bracket_ init close
