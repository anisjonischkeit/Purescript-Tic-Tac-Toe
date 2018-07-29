module TicTacToe where

import Data.Array ((!!), updateAt)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Either (Either(Left, Right))
import Prelude ((>>=))
import Data.Show
-- import Date.Show
  
data Piece = X | O

-- derive instance showPiece :: Show Piece
instance showPiece :: Show Piece where
  show = case _ of
    X -> "X"
    O -> "O"

invertPiece :: Piece -> Piece
invertPiece = case _ of
  X -> O
  O -> X

type Tile = Maybe Piece
type Row = Array Tile
type Board = Array Row

type Position = { x :: Int, y :: Int }

data PlacingError
  = NonExistantBoardPosition 
  | NonEmptyTileExistsAtPosition

initialBoard :: Board
initialBoard =
  [ [Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Nothing]
  ]

place :: Board -> Position -> Piece -> Either PlacingError Board
place board pos piece = newBoard
  where
    row :: Maybe Row
    row = board !! pos.y

    newRow :: Either PlacingError Row
    newRow = case row of 
      Nothing -> Left NonExistantBoardPosition
      Just r -> 
        case r !! pos.x of 
          Nothing -> Left NonExistantBoardPosition
          Just (Just _) -> Left NonEmptyTileExistsAtPosition
          Just Nothing -> 
            case updateAt pos.x (Just piece) r of
              Nothing -> Left NonExistantBoardPosition
              Just nr -> Right nr
      
    newBoard :: Either PlacingError Board
    newBoard = newRow >>= \nr -> 
      case updateAt pos.y nr board of
        Nothing -> Left NonExistantBoardPosition
        Just nb -> Right nb



test :: String
test = "hiiisssi"