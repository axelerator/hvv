{-# LANGUAGE MultiParamTypeClasses #-}
module Incrementor where

import Data.Cli


data Counter = Counter Int Bool

instance ProgramState Counter where
  finished (Counter _ b) = b

data ExPrg = ExPrg Counter

instance Program ExPrg Counter where
  cmds e = [ 
    ("q", (quit, "Quit")),
    ("+", (inc, "+1")),
    ("p", (printi, "print"))]

inc _ (Counter i f) = do
  return $ Counter (i+1) f

printi _ s@(Counter i _) = do
  putStrLn $ show i
  return s

quit :: Action Counter
quit _ (Counter i _) = do
  return $ Counter i True

idle _ a = do
  return a

ep :: ExPrg
ep = ExPrg $ Counter 0 False

ic :: Counter
ic = Counter 0 False


main = do
  runProg ep ic

