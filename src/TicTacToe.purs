module TicTacToe where

import Data.Show
import Prelude

import Data.Array ((!!), updateAt)
import Data.Either (Either(Left, Right))
import Data.Foldable (all, any)
import Data.Maybe (Maybe(Nothing, Just))
import Prelude ((>>=))
-- import Date.Show
  
data Piece = X | O

derive instance eqPiece :: Eq Piece
instance showPiece :: Show Piece where
  show = case _ of
    X -> "X"
    O -> "O"

invertPiece :: Piece -> Piece
invertPiece = case _ of
  X -> O
  O -> X

data BoardState
  = Playing
  | Winner Piece
  | Tie

allEq :: forall a. Eq a => a -> Array a -> Boolean
allEq comparator xs = all (eq comparator) xs

fullRow :: Board -> Piece -> Boolean
fullRow board piece = 
  any (allEq (Just piece)) board

fullColumn :: Board -> Piece -> Boolean
fullColumn board piece = 
  case board of
    [ [ Just a, _, _]
    , [ Just b, _, _]
    , [ Just c, _, _]
    ] -> allEq piece [a,b,c]
    [ [ _, Just a, _]
    , [ _, Just b, _]
    , [ _, Just c, _]
    ] -> allEq piece [a,b,c]
    [ [ _, _, Just a]
    , [ _, _, Just b]
    , [ _, _, Just c]
    ] -> allEq piece [a,b,c]
    _ -> false

fullDiagonal :: Board -> Piece -> Boolean
fullDiagonal board piece = 
  case board of
    [ [ Just a, _, _]
    , [ _, Just b, _]
    , [ _, _, Just c]
    ] -> allEq piece [a,b,c]
    [ [ _, _, Just a]
    , [ _, Just b, _]
    , [ Just c, _, _]
    ] -> allEq piece [a,b,c]
    _ -> false

fullBoard :: Board -> Boolean
fullBoard board = 
  case board of
    [ [ Just _, Just _, Just _]
    , [ Just _, Just _, Just _] 
    , [ Just _, Just _, Just _]
    ] -> true
    _ -> false

getBoardState :: Board -> BoardState
getBoardState board = 
  if (
    (fullRow board X) ||
    (fullColumn board X) ||
    (fullDiagonal board X)
  )
  then Winner X
  
  else if (
    (fullRow board O) ||
    (fullColumn board O) ||
    (fullDiagonal board O)
  )
  then Winner O

  else if fullBoard board
  then Tie

  else Playing

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