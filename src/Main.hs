module Main where

import Sound.Sequencer.Sequencer
import Sound.Sequencer.Vty
import Sound.Sequencer.Editor

import System.Environment (getArgs)

import Control.Monad

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs [] = run $ Editor (0, NoteCol) (0,0) 4 True defaultSeq 
parseArgs fn = mapM_ ((maybe (return ()) run =<<) . loadXM) fn

