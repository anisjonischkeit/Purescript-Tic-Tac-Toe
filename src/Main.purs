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
import TicTacToe (..)


-- Model


type Model = 
  { board :: Board 
  , player :: Piece
  }

initialModel :: Model
initialModel =
  { board : initialBoard
  , player : X
  }


-- Update


data Action
  = Play Position
  | Reset

update ∷ Model -> Action -> Model
update model action = 
  case _ of
    Play pos -> 
      case (boardState, piecePlacingResult) of
        (Playing, Right newBoard) ->
          { board : newBoard, player : invertPiece model.player }

        _ -> model
      
      where
        piecePlacingResult = place model.board pos model.player
        boardState = getBoardState model.board

    Reset -> initialModel


-- Views


tileView :: Tile -> Position -> Html Action
tileView tile pos = 
  H.button
    [ H.onClick (H.always_ (Play pos))
    , H.classes ["tictactoetile"]
    ]
    [ 
      H.text case tile of 
        Just p -> show p
        Nothing -> "."
    ]

boardView :: Board -> Html Action
boardView board =
  H.div 
    [] 
    (flip mapWithIndex board (\y row ->
      H.div [] (flip mapWithIndex row (\x tile ->
        tileView tile {x : x, y : y}
      ))
    ))

render ∷ Model -> Html Action
render model =
  H.div 
    [] 
    [ H.h1 [] [ H.text "Purescript Tic Tac Toe" ]
    , H.h2 [] [ H.text (showBoardState model) ]
    , boardView model.board
    , H.br []
    , H.button [ H.onClick (H.always_ Reset)] [ H.text "reset"]
    ]


-- Utils


showBoardState :: Model -> String
showBoardState model  =
  case getBoardState model.board of 
    Playing -> "It's " <> show model.player <> "'s turn."
    Winner piece -> show (invertPiece model.player) <> " has won the game."
    Tie -> "X and O have tied the game."


-- App


app ∷ PureApp Model Action
app = { update, render, init: initialModel }

main ∷ Effect Unit
main = void $ PureApp.makeWithSelector app "#app"