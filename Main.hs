{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.Binary
import Data.Binary.Get
import Data.DateTime
import Data.Word

import Data.Cli

data HvvTariff = Kurz | Nah | Gross | Day9
  deriving (Show, Eq, Ord)

data Ticket = Ticket {
  t_day :: Int,
  t_month :: Int,
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
  put (Ticket d t m) = put d >> put m  >> put t
  get = do
    d <- get
    m <- get
    t <- get
    return $ Ticket d m t

data AppState = AppState  {
  tickets :: [Ticket],
  fini :: Bool,
  day :: Int,
  month :: Int
} deriving (Show, Eq)


instance ProgramState AppState where
  finished state = fini state

data MainMenu = MainMenu

instance Program MainMenu AppState where
  cmds _ = comds

comds = [
      ("q", (quit, "Quit")),
      ("m", (setMonth, "<month> : set month")),
      ("+", (roll   1, "Roll day forward")),
      ("-", (roll (-1), "Roll day back")),
      ("n", (newT Nah, "add new kurz ticket")),
      ("k", (newT Kurz, "add new nah ticket")),
      ("g", (newT Gross, "add new gross ticket")),
      ("d", (newT Day9, "add new day ticket")),
      ("h", (printHelp, "print this help")),
      ("ls", (ls, "show tickets")),
      ("s", (save, "Save to file"))
    ]

ls _ as = do
  mapM (putStrLn . printTicket) (tickets as)
  return as


setMonth ps (AppState ts f day _) = do
  let newMonth = read ps
  putStrLn $ "Month is now: " ++ (show newMonth)
  return $ AppState ts f day newMonth

roll :: Int -> Action AppState
roll ff _ (AppState ts f d m) = do
    putStrLn $ show (d + ff)
    return $ AppState ts f (d + ff) m

quit _ (AppState ts _ d m) = do
  return (AppState ts True d m)

idle :: Action AppState
idle _ state = do
  return state

printHelp _ state = do
  mapM (\(c,(_,d)) -> putStrLn $ c ++ " - " ++ d) comds
  return state

printWelcome _ s = do
  putStrLn "Welcome!"
  return s

save :: Action AppState
save _ state@(AppState ts _ 0 0) = do
  encodeFile "hvv.data" ts
  putStrLn "saved."
  return state

newT tariff _ (AppState ts f day month) = do
  putStrLn $ "Hinzugefügt: " ++ (show day) ++ "." ++ (show month) ++ " " ++ (show tariff)
  return $ AppState (newTicket:ts) f day month
  where 
    newTicket = Ticket day month tariff 

value Nah = "1.30"
value Kurz = "1.80"
value Gross = "2.80"
value Day9 = "5.50"

printTicket (Ticket d m t ) = (show d) ++ "." ++ (show m) ++ ": " ++ (value t) ++ "€"

main = do
  runProg MainMenu (AppState [] False 0 0)
  return ()
