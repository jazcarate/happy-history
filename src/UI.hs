{-# LANGUAGE RankNTypes #-}

module UI where

import           RIO
import           RIO.List                       ( headMaybe )
import qualified RIO.Vector                    as V
import qualified Search                        as S
import qualified Types
import           UI.Fwk

data Focus = FLeft | FRight deriving (Eq)

data State = State
  { stateEditor   :: Editor
  , stateHistory  :: [Text]
  , stateSelected :: (Maybe [S.Match Text])
  , stateFocus    :: Focus
  }

-- TODO: TH?
stateEditorL :: Lens' State Editor
stateEditorL = lens stateEditor (\x y -> x { stateEditor = y })

stateFocusL :: Lens' State Focus
stateFocusL = lens stateFocus (\x y -> x { stateFocus = y })

stateSelectedL :: Lens' State (Maybe [S.Match Text])
stateSelectedL = lens stateSelected (\x y -> x { stateSelected = y })

mkState :: Text -> Vector Text -> State
mkState cmd history = State (mkEditor cmd) (V.toList history) Nothing FLeft -- TODO use vector throughout

prompt :: Bool -> Editor -> Widget
prompt focused editor = str mempty "`" <> ed <> str mempty "': "
 where
  ed = if focused then editorRender editor else str [] $ editorGetText editor

render :: State -> Widget
render s = prompt (stateFocus s == FLeft) (stateEditor s)
  <> maybe mempty (renderMatches) (stateSelected s)

renderMatches :: [S.Match Text] -> Widget
renderMatches ms =
  mconcat
    $   S.match
          (str [SetColor Foreground Vivid White, SetColor Background Dull Yellow])
          (str mempty)
    <$> ms

handleEventLensed :: Lens' a b -> EventHandler b ev -> EventHandler a ev
handleEventLensed l eh = \s ev -> do
  r <- eh (s ^. l) ev
  pure $ (\r1 -> s & l .~ r1) <$> r

handleEvent :: EventHandler State Event
handleEvent = handleFocus `andThen` handleType

handleFocus :: EventHandler State Event
handleFocus state ev = case ev of
  Control (CKLeft ) -> continue $ state & stateFocusL .~ FLeft
  Control (CKRight) -> continue $ state & stateFocusL .~ FRight
  _                 -> continue state

searchEH :: EventHandler State Event
searchEH state _ =
  continue
    $  state
    &  stateSelectedL
    .~ (headMaybe -- TODO Cursor for up and down
         (S.search (editorGetText $ state ^. stateEditorL) (stateHistory state))
       )

handleType :: EventHandler State Event
handleType state = case stateFocus state of
  FLeft  -> (handleEventLensed stateEditorL editorEH `andThen` searchEH) state
  FRight -> pure $ continue state

app :: App State
app = App { appDraw = render, appHandleEvent = handleEvent }

main :: State -> RIO Types.App (Maybe Text)
main initialState = do
  s <- start app initialState
  pure $ editorGetText . stateEditor <$> s
