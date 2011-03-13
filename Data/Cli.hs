{-# LANGUAGE MultiParamTypeClasses #-}
module Cli where

import Data.List

type Action a = (String -> a -> IO a)

class ProgramState a where
  finished :: a -> Bool

class ProgramState a => Program p a where
  initialState :: p -> a
  cmds :: p -> [(String, (Action a, String))]

data Counter = Counter Int Bool

instance ProgramState Counter where
  finished (Counter _ b) = b

data ExPrg = ExPrg Counter

instance Program ExPrg Counter where
  initialState (ExPrg c )= c
  cmds e = [ ("q", (quit, "Quit")) ]

quit :: Action Counter
quit _ (Counter i _) = do
  return $ Counter i True

idle _ a = do
  return a

ep :: Program ExPrg Counter => a
ep = ExPrg $ Counter 0 False

--foo :: Program p a => p -> Bool
foo prog = finished (initialState prog)


{-
main = do
  let ep = ExPrg $ Counter 0 False
  mainloop ep (initialState ep)

mainloop :: Program p a => p -> a -> IO a
mainloop p state = do
  case (finished state) of
    True -> return state
    False -> do
      input <- getLine
      let (ctrl, params) = splitAtC ' ' input
      let mbAction = lookup ctrl (cmds p)
      case mbAction of
        Nothing -> mainloop p state
        Just action -> do
          newstate <- (fst action) params state
          mainloop p newstate 

splitAtC c str = (h, rest t)
  where 
    cIdx = findIndex ((==) c) str
    (h,t) = case cIdx of
      Nothing -> (str, "")
      Just i -> splitAt i str
    rest "" = ""
    rest xs = tail xs

-}

