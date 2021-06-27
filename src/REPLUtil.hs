module REPLUtil where

import           RIO
import qualified RIO.ByteString                as B
import           RIO.Process
import qualified RIO.Text                      as T
import           Types
import           UI

test :: IO ()
test = do
  ctx <- mkProcessContext mempty
  let env = App
        { appLogFunc        = mempty
        , appProcessContext = ctx
        , appOptions        = Options { optionsLogFile    = Nothing
                                      , optionsInitialCmd = "bar"
                                      }
        }
  res <- runRIO env (main $ mkState "foo" mempty)
  case res of
    Just r  -> B.putStr $ T.encodeUtf8 r
    Nothing -> B.putStr "Aborted"
