module Main where

import Prelude

import Data.Array (mapWithIndex)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Spork.Html (Html)
import Spork.Html as H
import Spork.PureApp (PureApp)
import Spork.PureApp as PureApp
import TicTacToe (Board, Piece(X), Tile, Position, initialBoard, invertPiece, place)

type Model = 
  { board :: Board
  , player :: Piece
  }

initialModel :: Model
initialModel =
  { board : initialBoard
  , player : X
  }

data Action
  = Play Position
  | Noop

update ∷ Model → Action → Model
update model = case _ of
  Play pos → case place model.board pos model.player of 
    Right newBoard -> { board : newBoard, player : invertPiece model.player }
    Left e -> model
  Noop → model

-- tileView :: Tile -> ?
tileView :: Tile -> Position -> Html Action
tileView tile pos = 
  H.button
    [ H.onClick (H.always_ (Play pos)) ]
    [ 
      H.text case tile of 
        Just p -> show p
        Nothing -> "."
    ]

render ∷ Model → Html Action
render model =
  H.div 
    [] 
    (flip mapWithIndex model.board (\y row ->
      H.div [] (flip mapWithIndex row (\x tile ->
        tileView tile {x : x, y : y}
      ))
    ))

app ∷ PureApp Model Action
app = { update, render, init: initialModel }

main ∷ Effect Unit
main = void $ PureApp.makeWithSelector app "#app"