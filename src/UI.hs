{-# LANGUAGE RankNTypes #-}

module UI where

import           Cursor
import           RIO
import qualified Types
import           UI.Fwk

data Focus = FLeft | FRight

data State = State
  { stateEditor   :: Editor
  , stateHistory  :: Vector Text
  , stateSelected :: Maybe Text
  , stateFocus    :: Focus
  }

-- TODO: TH?
stateEditorL :: Lens' State Editor
stateEditorL = lens stateEditor (\x y -> x { stateEditor = y })

mkState :: Text -> Vector Text -> State
mkState cmd history = State (mkEditor cmd) history Nothing FLeft

render :: State -> Widget
render s =
  str mempty "`" <> editorRender (stateEditor s) <> str mempty "': " <> maybe
    mempty
    (str mempty)
    (stateSelected s)

handleEventLensed :: Lens' a b -> EventHandler b ev -> EventHandler a ev
handleEventLensed l eh = \s ev -> do
  r <- eh (s ^. l) ev
  pure $ (\r1 -> s & l .~ r1) <$> r


handleEvent :: EventHandler State Event
handleEvent state = case stateFocus state of
  FLeft  -> handleEventLensed stateEditorL editorEH state
  FRight -> undefined

app :: App State
app = App { appDraw = render, appHandleEvent = handleEvent }

main :: State -> RIO Types.App (Maybe Text)
main initialState = do
  s <- start app initialState
  pure $ getText . editorContent . stateEditor <$> s
