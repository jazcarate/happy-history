module Cursor where

import           RIO
import qualified RIO.Text                      as T


-- TODO: Text -> Bytecode?
data TextCursor = TextCursor
  { textCursorBefore  :: Text
  , textCursorCurrent :: Text
  , textCursorAfter   :: Text
  }


mkTextCursor :: Text -> TextCursor
mkTextCursor t = TextCursor t " " mempty

isTokenSplit :: Text -> Bool
isTokenSplit c = c == " "

nextToken :: TextCursor -> TextCursor
nextToken txc = loop $ next txc
 where
  loop (TextCursor _ c _) =
    if isTokenSplit c then txc else nextToken $ next txc

prevToken :: TextCursor -> TextCursor
prevToken txc = loop $ prev txc
 where
  loop (TextCursor _ c _) =
    if isTokenSplit c then txc else prevToken $ prev txc

next :: TextCursor -> TextCursor
next txc@(TextCursor b c a) =
  if T.length a < 1 then txc else TextCursor (b <> c) (T.take 1 a) (T.drop 1 a)

prev :: TextCursor -> TextCursor
prev txc@(TextCursor b c a) = if T.length b < 1
  then txc
  else TextCursor (T.dropEnd 1 b) (T.takeEnd 1 b) (c <> a)

home :: TextCursor -> TextCursor
home txc@(TextCursor b c a) = if T.length b < 1
  then txc
  else TextCursor mempty (T.take 1 b) (T.drop 1 b <> c <> a)

end :: TextCursor -> TextCursor
end txc = TextCursor (getText txc) " " mempty

getText :: TextCursor -> Text
getText (TextCursor b c a) = T.dropEnd 1 $ b <> c <> a

append :: Text -> TextCursor -> TextCursor
append txt (TextCursor b c a) = (TextCursor (b <> txt) c a)

delete :: TextCursor -> TextCursor
delete (TextCursor b c a) = TextCursor (T.dropEnd 1 b) c a
