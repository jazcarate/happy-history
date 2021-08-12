{-# LANGUAGE RankNTypes #-}

module UI where

import           Cursor.Vector
import qualified History                       as H
import           RIO
import qualified RIO.Vector                    as V
import qualified Search                        as S
import qualified Types
import           UI.Fwk

data Focus = FLeft | FRight deriving (Eq)

data State = State
  { statePrompt   :: Editor
  , stateCmd      :: Editor
  , stateHistory  :: [H.PastCommand]
  , stateSelected :: Cursor [S.Match Text]
  , stateFocus    :: Focus
  }

-- TODO: TH?
statePromptL :: Lens' State Editor
statePromptL = lens statePrompt (\x y -> x { statePrompt = y })

stateCmdL :: Lens' State Editor
stateCmdL = lens stateCmd (\x y -> x { stateCmd = y })

stateFocusL :: Lens' State Focus
stateFocusL = lens stateFocus (\x y -> x { stateFocus = y })

stateSelectedL :: Lens' State (Cursor [S.Match Text])
stateSelectedL = lens stateSelected (\x y -> x { stateSelected = y })

mkState :: Text -> Vector H.PastCommand -> State
mkState cmd history = State { statePrompt   = (mkEditor cmd)
                            , stateCmd      = mkEditor mempty
                            , stateHistory  = V.toList history
                            , stateSelected = mkCursor mempty
                            , stateFocus    = FLeft
                            }

prompt :: Bool -> Editor -> Widget
prompt focused editor = str mempty "`" <> ed <> str mempty "': "
 where
  ed = if focused then editorRender editor else str [] $ editorGetText editor

render :: State -> Widget
render s = prompt (stateFocus s == FLeft) (statePrompt s)
  <> renderMatches (stateSelected s)

renderMatches :: Cursor [S.Match Text] -> Widget
renderMatches curr = case getSelected curr of
  Just val ->
    mconcat
      $   S.match
            (str
              [SetColor Foreground Vivid White, SetColor Background Dull Yellow]
            )
            (str mempty)
      <$> val
  Nothing -> mempty

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
    .~ (mkCursor -- TODO Cursor for up and down
         ( V.fromList
         $ (S.search (editorGetText $ state ^. statePromptL)
                     (H.command <$> stateHistory state)
           )
         )
       )

handleType :: EventHandler State Event
handleType state = case stateFocus state of
  FLeft  -> (handleEventLensed statePromptL editorEH `andThen` searchEH) state
  FRight -> handleEventLensed stateSelectedL handleSelected state

handleSelected :: EventHandler (Cursor a) Event
handleSelected state ev = continue $ case ev of
  Control (CKUp) -> next state
  Control CKDown -> prev state
  _              -> state


app :: App State
app = App { appDraw = render, appHandleEvent = handleEvent }

main :: State -> RIO Types.App (Maybe Text)
main initialState = do
  state <- start app initialState
  pure
    $   (\s -> mconcat $ S.match id id <$> s)
    <$> (getSelected . stateSelected =<< state)
