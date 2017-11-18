{-# LANGUAGE RecordWildCards #-}

module Sound.Sequencer.Editor where

import Sound.Sequencer.Sequencer

import qualified Codec.Tracker.XM as XM
import Codec.Tracker.XM.Pattern (Cell(..), patternData)
import Codec.Tracker.XM.Header (numChannels)
import Codec.Tracker.Common

import Control.Exception
import Control.Arrow
import Control.Monad
import Data.Binary.Get
import Data.Char
import Data.Maybe
import Data.List.Split
import qualified Data.ByteString.Lazy as B
import System.IO

data Column = NoteCol | InsCol16 | InsCol1 | VolCol16 | VolCol1 | ETCol | EPCol16 | EPCol1
  deriving (Show, Eq, Enum)

data Editor = Editor { cursorX :: (Int, Column)
                     , cursorY :: (Int, Int)
                     , octave  :: Int
                     , running :: Bool
                     , sqncr   :: Sequencer
                     }

data EditorEvent = MoveUp | MoveDown | MoveRight | MoveLeft | JumpUp | JumpDown | JumpRight | JumpLeft | SelectOctave Int | Edit Char | Delete | AddRow | RemoveRow | Quit
  deriving (Show, Eq)

loadXM :: String -> IO (Maybe Editor)
loadXM fn = try (B.readFile fn) >>= \h ->
  case h of
    Left e -> hPrint stderr (e :: IOException) >> return Nothing
    Right f -> case runGetOrFail XM.getModule f of
        Left  (_,n,e) -> hPutStrLn stderr (show n ++ ": " ++ e) >> return Nothing
        Right (_,_,m) -> return $ Just (fromXM m)

fromXM :: XM.Module -> Editor
fromXM XM.Module{..} = Editor (0, NoteCol) (0,0) 4 True (defaultSeq { patterns = [ ("", chunksOf n $ patternData p) | p <- patterns ] })
  where n = fromIntegral $ numChannels header

edit :: Editor -> EditorEvent -> Editor
edit ed@Editor{..} ev = case ev of
    MoveUp         -> if y2 > 0 then ed { cursorY = (y1, y2 - 1) }
                                else ed { cursorY = (y1, numRows - 1) }
    MoveDown       -> if y2 < numRows - 1 then ed { cursorY = (y1, y2 + 1) }
                                          else ed { cursorY = (y1, 0) }
    MoveRight      -> if cursorX == (numCh - 1, EPCol1) then ed { cursorX = (0, NoteCol) }
                                                        else ed { cursorX = next cursorX }
    MoveLeft       -> if cursorX == (0, NoteCol) then ed { cursorX = (numCh - 1, EPCol1) }
                                                 else ed { cursorX = prev cursorX }
    JumpUp         -> if y1 == 0 then ed { cursorY = (numPat - 1, 0) }
                                 else ed { cursorY = (y1 - 1, 0) }
    JumpDown       -> if y1 == numPat - 1 then ed { cursorY = (0, 0) }
                                          else ed { cursorY = (y1 + 1, 0) }
    JumpRight      -> if x1 == numCh - 1 then ed { cursorX = (0, x2) }
                                         else ed { cursorX = (x1 + 1, x2) }
    JumpLeft       -> if x1 == 0 then ed { cursorX = (numCh - 1, x2) }
                                 else ed { cursorX = (x1 - 1, x2) }
    SelectOctave i -> if i < 10 then ed { octave = i }
                                else ed
    AddRow         -> ed { sqncr = sqncr { patterns = updateNth y1 (second (++ [replicate numCh emptyCell])) (patterns sqncr) }}
    RemoveRow      -> ed { sqncr = sqncr { patterns = updateNth y1 (second rr) (patterns sqncr) }}
    Quit           -> ed { running = False }
    Delete         -> patternEdit ed '\DEL'
    Edit c         -> patternEdit ed c
  where numRows = length $ snd $ patterns sqncr !! y1
        numCh   = length . head . snd $ patterns sqncr !! y1
        numPat  = length $ patterns sqncr
        (x1,x2) = cursorX
        (y1,y2) = cursorY
        next (a, col) = if col == EPCol1  then (a+1, NoteCol) else (a, succ col)
        prev (a, col) = if col == NoteCol then (a-1, EPCol1)  else (a, pred col)
        rr (x:xs) = if xs == [] then [x] else init (x:xs)


modifyCurrentCell :: Editor -> (Cell -> Cell) -> Editor
modifyCurrentCell ed@Editor{..} f = ed { sqncr = sqncr {
    patterns = updateNth y1 (\(n,p) -> (n, updateNth y2 (updateNth x1 f) p)) (patterns sqncr) }}
  where (x1,x2) = cursorX
        (y1,y2) = cursorY

updateNth :: Int -> (a -> a) -> [a] -> [a]
updateNth n u (x:xs)
  | n == 0    = u x : xs
  | otherwise = x : updateNth (n - 1) u xs


patternEdit :: Editor -> Char -> Editor
patternEdit ed@Editor{..} '\DEL' =
    case snd cursorX of
      NoteCol  -> modifyCurrentCell ed (\a -> a { note = Nothing })
      InsCol16 -> modifyCurrentCell ed (\a -> a { instrument = Nothing })
      InsCol1  -> modifyCurrentCell ed (\a -> a { instrument = Nothing })
      VolCol16 -> modifyCurrentCell ed (\a -> a { volume = Nothing })
      VolCol1  -> modifyCurrentCell ed (\a -> a { volume = Nothing })
      ETCol    -> modifyCurrentCell ed (\a -> a { effectType = Nothing })
      EPCol16  -> modifyCurrentCell ed (\a -> a { effectParam = Nothing })
      EPCol1   -> modifyCurrentCell ed (\a -> a { effectParam = Nothing })
patternEdit ed@Editor{..} c =
    case snd cursorX of
      NoteCol  -> case lookup c (pianoRoll octave) of
                         Just x  -> modifyCurrentCell ed (\a -> a { note = Just x })
                         Nothing -> ed
      InsCol16 -> hexedit
                    (\a -> a { instrument = Just $ set16th (fromMaybe 0 $ instrument a) })
      InsCol1  -> hexedit
                    (\a -> a { instrument = Just $ set1st  (fromMaybe 0 $ instrument a) })
      VolCol16 -> hexedit
                    (\a -> a { volume = Just $ set16th (fromMaybe 0 $ volume a) })
      VolCol1  -> hexedit
                    (\a -> a { volume = Just $ set1st  (fromMaybe 0 $ volume a) })
      ETCol    -> hexedit
                    (\a -> a { effectType = Just . fromIntegral . digitToInt $ c })
      EPCol16  -> hexedit
                    (\a -> a { effectParam = Just $ set16th (fromMaybe 0 $ effectParam a) })
      EPCol1   -> hexedit
                    (\a -> a { effectParam = Just $ set1st  (fromMaybe 0 $ effectParam a) })
  where hexedit f = if isHexDigit c then modifyCurrentCell ed f else ed
        set16th n = fromIntegral (digitToInt c) * 16 + (n `mod` 16)
        set1st  n = n - (n `mod` 16) + fromIntegral (digitToInt c)


-- TODO: support different layouts
--       (maybe query xkb?)
pianoRoll :: Int -> [(Char, Note)]
pianoRoll o = fmap (Note <$>)
  [ ('z', Pitch C o), ('s', Pitch Csharp o), ('x', Pitch D o), ('d', Pitch Dsharp o)
  , ('c', Pitch E o), ('v', Pitch F o), ('g', Pitch Fsharp o), ('b', Pitch G o)
  , ('h', Pitch Gsharp o), ('n', Pitch A o), ('j', Pitch Asharp o)
  ]

