module Numeric.Dopamine.Outcome where

data Outcome o r = Outcome { observation :: o, reward :: r } deriving (Show)

