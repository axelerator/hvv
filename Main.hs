module Main where

import Data.Binary
import Data.Binary.Get
import Data.DateTime
import Data.Word

data HvvTariff = Kurz | Nah | Gross | Day9
  deriving (Show, Eq, Ord)

data Ticket = Ticket {
  date :: DateTime,
  tariff :: HvvTariff
} deriving (Eq, Show)

instance Binary HvvTariff where
  put Kurz = put (0::Word8)
  put Nah = put (1::Word8)
  put Gross = put (2::Word8)
  put Day9 = put (3::Word8)

  get = do
    i <- getWord8
    case i of
      0 -> return Kurz
      1 -> return Nah
      2 -> return Gross
      3 -> return Day9
  
instance Binary Ticket where
  put (Ticket d t) = put (toSeconds d) >> put t
  get = do
    d <- get
    t <- get
    return $ Ticket (fromSeconds d) t

commandos :: [(Char, (Action, String))]
commandos = [
    ('q', (quit, "Quit")),
    ('h', (printHelp, "print this help")),
    ('s', (save, "Save to file"))
  ]

quit _ (AppState ts _) = do
  return (AppState ts True)

idle :: Action
idle _ state = do
  return state

printHelp _ state = do
  mapM (\(c,(_,d)) -> putStrLn $ [c] ++ " - " ++ d) commandos
  return state

printWelcome _ s = do
  putStrLn "Welcome!"
  return s

save :: Action
save _ state@(AppState ts _) = do
  encodeFile "hvv.data" ts
  putStrLn "saved."
  return state

data AppState = AppState  {
  tickets :: [Ticket],
  finished :: Bool
} deriving (Show, Eq)

type Action = (String -> AppState -> IO AppState)

mainloop :: AppState -> Action -> String -> IO AppState
mainloop state action params = do
  newstate <- action params state
  case (finished newstate) of
    True -> return newstate
    False -> do
      (c:nxtParams) <- getLine
      let nextAct = lookup c commandos
      case nextAct of
        Nothing -> printHelp "" state
        Just a -> mainloop newstate (fst a) nxtParams

main = do
  mainloop (AppState [] False) printWelcome ""
