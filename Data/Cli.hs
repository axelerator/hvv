{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cli where

import Data.List

type Action a = (String -> a -> IO a)

class ProgramState a where
  finished :: a -> Bool

class ProgramState a => Program p a where
  cmds :: p -> [(String, (Action a, String))]

runProg :: (Program p b) => p -> b -> IO b
runProg p state = do
  case (finished state) of
    True -> return state
    False -> do
      input <- getLine
      let (ctrl, params) = splitAtC ' ' input
      let mbAction = lookup ctrl (cmds p)
      case mbAction of
        Nothing -> runProg p state
        Just action -> do
          newstate <- (fst action) params state
          runProg p newstate 

splitAtC c str = (h, rest t)
  where 
    cIdx = findIndex ((==) c) str
    (h,t) = case cIdx of
      Nothing -> (str, "")
      Just i -> splitAt i str
    rest "" = ""
    rest xs = tail xs


