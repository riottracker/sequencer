{-# LANGUAGE RecordWildCards #-}

module Sound.Sequencer.Vty where

import Data.Char
import Data.Maybe
import Graphics.Vty
import Control.Monad
import Sound.Sequencer.Sequencer

data Column = NoteCol | InsCol16 | InsCol1 | VolCol16 | VolCol1 | ETCol | EPCol16 | EPCol1
  deriving (Show, Eq, Enum)

data Editor = Editor { vty      :: Vty
                     , sqncr    :: Sequencer
                     , cursorX  :: (Int, Column)
                     , cursorY  :: (Int, Int)
                     , octave   :: Int
                     , playing  :: Bool
                     , running  :: Bool
                     }

rootImage :: Editor -> Image
rootImage Editor{..} = foldr1 (<|>) $ (renderIndex : [ renderChannel i | i <- [0 .. numChannels-1]]) <*> pure Editor{..}
  where numChannels = length $ head (patterns sqncr !! fst cursorY)

renderIndex :: Editor -> Image
renderIndex Editor{..} = pad 0 0 2 0 $ foldr1 (<->) [ row i | i <- [1 .. length curPat]]
  where curPat = patterns sqncr !! fst cursorY
        row  i = string (defAttr `withForeColor` brightYellow) (replicate (4 - length (show i)) '0' ++ show i)

renderChannel :: Int -> Editor -> Image
renderChannel ch Editor{..} = pad 1 0 1 0 $ foldr1 (<->) [ renderCell Editor{..} ch i | i <- [0 .. length curPat - 1]]
  where curPat = patterns sqncr !! fst cursorY

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
  where cell        = patterns sqncr !! fst cursorY !! i !! ch
        w c t       = (if cursorX == (ch, c) && snd cursorY == i then se else id) $ defAttr `withForeColor` t
        se  x       = if playing then x `withStyle` reverseVideo else x `withBackColor` red
        print1st a  = if isNothing a then "." else [ intToDigit . fromIntegral $ fromJust a `mod` 16 ]
        print16th a = if isNothing a then "." else [ intToDigit . fromIntegral $ fromJust a `div` 16 ]


main :: IO ()
main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  let ed = Editor vty defaultSeq (0, NoteCol) (0, 0) 4 False True
  mainLoop ed   
  shutdown vty

mainLoop :: Editor -> IO ()
mainLoop Editor{..} = do
  update vty $ picForImage (rootImage Editor{..})
  when running
    (mainLoop =<< handleEvents Editor{..})

handleEvents :: Editor -> IO Editor
handleEvents ed@Editor{..} = do
    ev <- nextEvent vty
    case ev of
      EvKey KEsc        [] -> return ed { running = False }
      EvKey (KFun i)    [] -> return ed { octave = if i < 10 then i else octave }
      EvKey (KChar c)   [] -> return $ edit ed c
      EvKey KDel        [] -> return $ edit ed '\DEL'
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
  where numRows       = length $ patterns sqncr !! fst cursorY
        numChns       = length . head $ patterns sqncr !! fst cursorY
        next (a, col) = if col == EPCol1  then (a+1, NoteCol) else (a, succ col)
        prev (a, col) = if col == NoteCol then (a-1, EPCol1)  else (a, pred col)

edit :: Editor -> Char -> Editor
edit ed@Editor{..} '\DEL' =
    case snd cursorX of
      NoteCol  -> modifyCurrentCell ed (\a -> a { note = Nothing })
      InsCol16 -> modifyCurrentCell ed (\a -> a { instrument = Nothing })
      InsCol1  -> modifyCurrentCell ed (\a -> a { instrument = Nothing })
      VolCol16 -> modifyCurrentCell ed (\a -> a { volume = Nothing })
      VolCol1  -> modifyCurrentCell ed (\a -> a { volume = Nothing })
      ETCol    -> modifyCurrentCell ed (\a -> a { effectType = Nothing })
      EPCol16  -> modifyCurrentCell ed (\a -> a { effectParam = Nothing })
      EPCol1   -> modifyCurrentCell ed (\a -> a { effectParam = Nothing })
edit ed@Editor{..} c    =
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

modifyCurrentCell :: Editor -> (Cell -> Cell) -> Editor
modifyCurrentCell ed@Editor{..} f = ed { sqncr = sqncr { patterns =
    updateNth (fst cursorY)
      (updateNth (snd cursorY)
        (updateNth (fst cursorX) f))
       (patterns sqncr)
    }}
  where updateNth n u (x:xs)
          | n == 0    = u x : xs
          | otherwise = x : updateNth (n - 1) u xs


-- TODO: support different layouts
--       (maybe query xkb?)
pianoRoll :: Int -> [(Char, Note)]
pianoRoll o = fmap (Note <$>)
  [ ('z', Pitch C o), ('s', Pitch Csharp o), ('x', Pitch D o), ('d', Pitch Dsharp o)
  , ('c', Pitch E o), ('v', Pitch F o), ('g', Pitch Fsharp o), ('b', Pitch G o)
  , ('h', Pitch Gsharp o), ('n', Pitch A o), ('j', Pitch Asharp o) 
  ]

