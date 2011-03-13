{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.Binary
import Data.Binary.Get
import Data.DateTime
import Data.Word
import System.IO

import Data.Cli

data HvvTariff = Kurz | Nah | Gross | Day9
  deriving (Show, Eq, Ord, Enum)

data Ticket = Ticket {
  t_day :: Int,
  t_month :: Int,
  tariff :: HvvTariff
} deriving (Eq, Show)

instance Binary HvvTariff where
  put t = put $ fromEnum t

  get = do
    i <- get
    return $ toEnum i
  
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
  cmds _ = map wrap comds

wrap :: (String, (Action AppState, String))-> (String, (Action AppState, String))
wrap (s, (action,d)) = (s, (prompt action, d))

comds = [
      ("q", (quit, "Quit")),
      ("m", (setMonth, "<month> : set month")),
      ("+", (roll   1, "Roll day forward")),
      ("-", (roll (-1), "Roll day back")),
      ("1", (newT Nah, "add new kurz ticket")),
      ("2", (newT Kurz, "add new nah ticket")),
      ("3", (newT Gross, "add new gross ticket")),
      ("4", (newT Day9, "add new day ticket")),
      ("h", (printHelp, "print this help")),
      ("e", (export, "<filename> export to file")),
      ("ls", (ls, "show tickets")),
      ("l", (load, "<filename> load from file")),
      ("s", (save, "<filename> Save to file"))
    ]

ls _ as = do
  mapM (putStrLn . printTicket) (reverse $ tickets as)
  return as

export filename state = do
  mapM (putStrLn . exportTicket) (reverse $ tickets state) 
  return state


setMonth ps (AppState ts f day _) = do
  let newMonth = read ps
  return $ AppState ts f day newMonth

roll :: Int -> Action AppState
roll ff _ (AppState ts f d m) = do
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
save filename state = do
  encodeFile filename (tickets state)
  putStrLn "saved."
  return state

load :: Action AppState
load filename (AppState _ f d m) = do
  ts <- decodeFile filename 
  putStrLn "load."
  return $ AppState ts f d m

newT tariff _ (AppState ts f day month) = do
  putStrLn $ "Hinzugefügt: " ++ (show day) ++ "." ++ (show month) ++ " " ++ (show tariff)
  return $ AppState (newTicket:ts) f day month
  where 
    newTicket = Ticket day month tariff 

value Nah = "1,30"
value Kurz = "1,80"
value Gross = "2,80"
value Day9 = "5,50"

prompt action ps as@(AppState ts f d m )= do
  newState@(AppState _ _ nd nm ) <- action ps as
  putStr $ "### " ++ (zls nd) ++ "." ++ (zls nm) ++ " -> "
  hFlush stdout
  return newState

printTicket (Ticket d m t ) = (show d) ++ "." ++ (show m) ++ ": " ++ (value t) ++ "€"

exportTicket (Ticket d m t) = ";\"H\";" ++ (value t) ++ ";;8663;;;" ++ (zls d) ++ (zls m) ++ ";79903;;;;;\"HVV\""

zls d
      | d < 10 = "0" ++ (show d)
      | otherwise = show d  


main = do
  runProg MainMenu (AppState [] False 1 1)
  return ()
