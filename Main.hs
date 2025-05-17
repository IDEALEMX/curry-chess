module Main where

import Board
import Data.Char (digitToInt)

emptyBoardColumn :: Column
emptyBoardColumn = replicate 8 Empty 

backrank :: Color -> Column
backrank color = 
    [ Piece { pieceType = Rook,   color = color }
    , Piece { pieceType = Knight, color = color }
    , Piece { pieceType = Bishop, color = color }
    , Piece { pieceType = Queen,  color = color }
    , Piece { pieceType = King,   color = color }
    , Piece { pieceType = Bishop, color = color }
    , Piece { pieceType = Knight, color = color }
    , Piece { pieceType = Rook,   color = color } ]

getDefaultStartingBoard :: Board
getDefaultStartingBoard = Board
    [ backrank Red
    , replicate 8 Piece {pieceType=Pawn, color=Red}
    , emptyBoardColumn
    , emptyBoardColumn
    , emptyBoardColumn
    , emptyBoardColumn
    , replicate 8 Piece {pieceType=Pawn, color=Blue}
    , backrank Blue ]

--isPawnInDefaultPosition :: Board -> Int ->

coordinatesParser :: String -> (Int, Int)
coordinatesParser string = (3, firstCoord)
    where
        firstCoord :: Int
        firstCoord = digitToInt $ string !! 1

parseMove :: String -> Maybe Move
parseMove inputString
    | inputString == "" = Nothing
    | inputString == "O-O" = Just ShortCastle
    | inputString == "O-O-O" = Just LongCastle
    | inputSize == 2 = Just ShortCastle
    where
        inputSize = length inputString

main :: IO()
main = print getDefaultStartingBoard
