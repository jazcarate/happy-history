{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import           Import
import           Options.Applicative.Simple
import qualified Paths_happy_history
import           RIO.Process
import           Run


import           Prelude                        ( getContents
                                                , putStr
                                                , putStrLn
                                                , writeFile
                                                )
import           RIO.List                       ( intercalate )
import           System.Console.ANSI
import           System.Environment
import           System.Process

{- main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_happy_history.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run -}

main :: IO ()
main = testRewrite

testRewrite :: IO ()
testRewrite = do
  cursorUp 1
  saveCursor
  clearFromCursorToScreenEnd
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
  putStr
    "Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of de Finibus Bonorum et Malorum (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, Lorem ipsum dolor sit amet, comes from a line in section 1.10.32.\nThe standard chunk of Lorem Ipsum used since the 1500s is reproduced below for those interested. Sections 1.10.32 and 1.10.33 from de Finibus Bonorum et Malorum by Cicero are also reproduced in their exact original form, accompanied by English versions from the 1914 translation by H. Rackham."
  hFlush stdout
  threadDelay 1500000
  restoreCursor
  clearFromCursorToScreenEnd
  setSGR [Reset]
  hFlush stdout

  args <- getArgs
  let cmd = intercalate " " args
  writeFile "/tmp/.hh_last_command" cmd



testBind :: IO ()
testBind = do
  args <- getArgs
  putStrLn "Args:"
  putStrLn $ show args

  input <- getContents
  putStrLn "Stdin:"
  putStrLn $ input
