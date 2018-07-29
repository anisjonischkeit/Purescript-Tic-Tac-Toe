module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import TicTacToe (test)

main :: Effect Unit
main = do
  log test
