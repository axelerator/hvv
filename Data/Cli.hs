{-# LANGUAGE MultiParamTypeClasses #-}
module Cli where

import Data.List

{-
class Commando a where
  description :: a -> String
--  action :: String -> a -> IO a

-}

class ProgramState a where
  finished :: a -> Bool



type Action a = (String -> a -> IO a)

class Program p a where
  initialState :: p a -> a
  cmds :: p a -> [(String, Action a)]


data Counter = Counter Int Bool
instance ProgramState Counter where
  finished (Counter _ b) = b

data ExPrg = ExPrg Counter


instance Program ExPrg Counter where
  initialState = undefined
  cmds = undefined
  --initialState = Counter 0 False
  --cmds = [()]  

{-

mainloop :: a -> Commando -> String -> IO a
mainloop state cmd params = do
  newstate <- action params state
  case (finished newstate) of
    True -> return newstate
    False -> do
      input <- getLine
      (ctrl, params)
      let nextAct = lookup c commandos
      case nextAct of
        Nothing -> printHelp "" state
        Just a -> mainloop newstate (fst a) nxtParams
-}

splitAtC c str = (h, rest t)
  where 
    cIdx = findIndex ((==) c) str
    (h,t) = case cIdx of
      Nothing -> (str, "")
      Just i -> splitAt i str
    rest "" = ""
    rest xs = tail xs


