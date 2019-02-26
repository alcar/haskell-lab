module RPS where

data Choice = Rock  | Paper | Scissors
data Result = Win   | Loss  | Draw      deriving Show

play :: Choice -> Choice -> Result
play Rock       Rock        = Draw
play Rock       Paper       = Loss
play Rock       Scissors    = Win 
play Paper      Paper       = Draw
play Paper      Scissors    = Loss 
play Scissors   Scissors    = Draw
play a          b           = invertValue (flip play a b) where
    invertValue Win     = Loss
    invertValue Loss    = Win
