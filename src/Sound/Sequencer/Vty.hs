{-# LANGUAGE RecordWildCards #-}

module Sound.Sequencer.Vty where

import Data.Char
import Data.Maybe
import Graphics.Vty

import Sound.Sequencer.Sequencer

data Column = NoteCol | InsCol16 | InsCol1 | VolCol16 | VolCol1 | ETCol | EPCol16 | EPCol1
  deriving (Show, Eq, Enum)

data Editor = Editor { vty      :: Vty
                     , seq      :: Sequencer
                     , cursorX  :: (Int, Column)
                     , cursorY  :: (Int, Int)
                     , editMode :: Bool
                     , playing  :: Bool
                     , running  :: Bool
                     }

rootImage :: Editor -> Image
rootImage Editor{..} = foldr1 (<|>) $ (renderIndex : [ renderChannel i | i <- [0 .. numChannels-1]]) <*> pure Editor{..}
  where numChannels = length $ head (patterns seq !! fst cursorY)

renderIndex :: Editor -> Image
renderIndex Editor{..} = pad 0 0 2 0 $ foldr1 (<->) [ row i | i <- [1 .. length curPat]]
  where curPat = patterns seq !! fst cursorY
        row  i = string (defAttr `withForeColor` brightYellow) (replicate (4 - length (show i)) '0' ++ show i)

renderChannel :: Int -> Editor -> Image
renderChannel ch Editor{..} = pad 1 0 1 0 $ foldr1 (<->) [ renderCell Editor{..} ch i | i <- [0 .. length curPat - 1]]
  where curPat = (patterns seq) !! fst cursorY

renderCell :: Editor -> Int -> Int -> Image
renderCell Editor{..} ch i = string (w NoteCol brightWhite) (maybe "..." show (note cell))
                         <|> string defAttr " "
                         <|> (string (w InsCol16 cyan) (print16th $ instrument cell))
                         <|> (string (w InsCol1 cyan)  (print1st  $ instrument cell))
                         <|> string defAttr " "
                         <|> (string (w VolCol16 green) (print16th $ volume cell))
                         <|> (string (w VolCol1 green)  (print1st  $ volume cell))
                         <|> string defAttr " "
                         <|> (string (w ETCol yellow) (print1st $ effectType cell))
                         <|> (string (w EPCol16 magenta) (print16th $ effectParam cell))
                         <|> (string (w EPCol1 magenta) (print1st $ effectParam cell))
                         <|> string defAttr " "
  where cell        = patterns seq !! fst cursorY !! i !! ch
        w c t       = (if cursorX == (ch, c) && snd cursorY == i then se else id) $ defAttr `withForeColor` t
        se  x       = if editMode then x `withBackColor` red else x `withStyle` reverseVideo
        print1st a  = if isNothing a then "." else [ intToDigit . fromIntegral $ fromJust a `mod` 16 ]
        print16th a = if isNothing a then "." else [ intToDigit . fromIntegral $ fromJust a `div` 16 ]


main :: IO ()
main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  let ed = Editor vty defaultSeq (0, NoteCol) (0, 0) True False True
  mainLoop ed   
  shutdown vty

mainLoop :: Editor -> IO ()
mainLoop Editor{..} = do
  update vty $ picForImage (rootImage Editor{..})
  case running of
    True -> mainLoop =<< handleEvents Editor{..}
    _    -> return ()

handleEvents :: Editor -> IO Editor
handleEvents ed@Editor{..} = do
    ev <- nextEvent vty
    case ev of
      EvKey KEsc        [] -> return ed { running = False }
      EvKey KUp         [] -> if snd cursorY > 0 then
                                return ed { cursorY = (fst cursorY, snd cursorY - 1) }
                              else
                                return ed { cursorY = (fst cursorY, numRows - 1) }
      EvKey KDown       [] -> if snd cursorY < numRows - 1 then
                                return ed { cursorY = (fst cursorY, snd cursorY + 1) }
                              else
                                return ed { cursorY = (fst cursorY, 0) }
      EvKey KRight      [] -> if cursorX == (numChns - 1, EPCol1) then
                                return ed { cursorX = (0, NoteCol) }
                              else
                                return ed { cursorX = next cursorX }
      EvKey KRight [MCtrl] -> if fst cursorX == numChns - 1 then
                                return ed { cursorX = (0, snd cursorX) }
                              else
                                return ed { cursorX = (fst cursorX + 1, snd cursorX) }
      EvKey KLeft       [] -> if cursorX == (0, NoteCol) then
                                return ed { cursorX = (numChns - 1, EPCol1) }
                              else
                                return ed { cursorX = prev cursorX }
      EvKey KLeft  [MCtrl] -> if fst cursorX == 0 then
                                return ed { cursorX = (numChns - 1, snd cursorX) }
                              else
                                return ed { cursorX = (fst cursorX - 1, snd cursorX) }
      _                    -> return ed
  where numRows       = length $ (patterns seq) !! fst cursorY
        numChns       = length . head $ (patterns seq) !! fst cursorY
        next (a, col) = if col == EPCol1  then (a+1, NoteCol) else (a, succ col)
        prev (a, col) = if col == NoteCol then (a-1, EPCol1)  else (a, pred col)
