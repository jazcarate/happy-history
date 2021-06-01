module Run
  ( run
  ) where

import           Data.Char                      ( showLitChar )
import           Import
import           Prelude                        ( getChar
                                                , putStr
                                                )
import qualified RIO.Text

run :: RIO Import.App ()
run = liftIO run'
 where
  run' :: IO ()
  run' = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    key <- getKey
    when (key /= "\ESC") $ do
      case key of
        "\n"   -> putStr "⎆"
        "\DEL" -> putStr "⎋"
        _ ->
          putStr
            $  RIO.Text.unpack
            $  "<"
            <> RIO.Text.concatMap (\c -> RIO.Text.pack $ showLitChar c " ") key -- https://github.com/jtdaugherty/vty/blob/master/src/Graphics/Vty/Input/Terminfo/ANSIVT.hs
            <> ">"
      run'

getKey :: IO Text
getKey = RIO.Text.reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) $ char `RIO.Text.cons` chars
