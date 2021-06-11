module UI where

import           RIO
import qualified Types
import           UI.Fwk

type State = Editor

app :: App State
app = App { appDraw = editorRender, appHandleEvent = editorEH }

main :: Text -> RIO Types.App (Maybe State)
main t = start app (mkEditor t)
