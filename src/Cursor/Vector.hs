module Cursor.Vector where

import           RIO
import qualified RIO.Vector                    as V

data Cursor a = Cursor
  { cursorData     :: Vector a
  , cursorSelected :: Maybe Int
  }

mkCursor :: Vector a -> Cursor a
mkCursor v = Cursor v (if V.null v then Nothing else Just 0)

next :: Cursor a -> Cursor a
next (Cursor v selected) = Cursor v newSelected
 where
  newSelected = case selected of
    Just val -> if val >= V.length v - 1 then Just val else Just (val + 1)
    Nothing  -> if V.null v then Nothing else Just 0

prev :: Cursor a -> Cursor a
prev (Cursor v selected) = Cursor v newSelected
 where
  newSelected = case selected of
    Just val -> if val > 0 then Just (val - 1) else Just val
    Nothing  -> if V.null v then Nothing else Just (V.length v)

getSelected :: Cursor a -> Maybe a
getSelected (Cursor v selected) = (v !!) <$> selected
 where
  (!!) :: V.Vector v a => v a -> Int -> a
  vector !! i =
    maybe (error "Should never have selected a cursor from outside the range")
          id
      $    vector
      V.!? i

