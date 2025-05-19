module Main where

import ValidMoves
import Board

import Data.Char (digitToInt, ord)
import Data.Binary.Builder (flush)
import System.IO

emptyBoardRow :: Row
emptyBoardRow = replicate 8 Empty 

backrank :: Color -> Row
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
    , emptyBoardRow
    , emptyBoardRow
    , emptyBoardRow
    , emptyBoardRow
    , replicate 8 Piece {pieceType=Pawn, color=Blue}
    , backrank Blue ]

pawnDefaultPosition :: Coordinates -> Color -> Coordinates
pawnDefaultPosition (y, x) Red = (1, x)
pawnDefaultPosition (y, x) Blue = (6, x)

isPawnInDefaultPosition :: Board -> Coordinates -> Color -> Bool
isPawnInDefaultPosition board (y,x) Red
    | y /= 3 = False
    | isEmpty pawnStartSquare || pieceType pawnStartSquare /= Pawn = False
    | otherwise = True
    where
        pawnStartSquare = getSquareFromCoordinates board (pawnDefaultPosition (y,x) Red)

isPawnInDefaultPosition board (y,x) Blue
    | y /= 4 = False
    | isEmpty pawnStartSquare || pieceType pawnStartSquare /= Pawn = False
    | otherwise = True
    where
        pawnStartSquare = getSquareFromCoordinates board (pawnDefaultPosition (y,x) Blue)

coordinatesParser :: String -> Coordinates
coordinatesParser string = (firstCoord, secondCoord)
    where
        firstCoord = 8 - digitToInt (string !! 1)
        secondCoord = ord (string !! 0) - 97

parseMove :: String -> Move
parseMove inputString
    | inputString == "O-O" = ShortCastle
    | inputString == "O-O-O" = LongCastle
    | otherwise = SingleMove (startCoords, endCoords)
    where
        startingString = take 2 inputString
        endingString = reverse $ take 2 $ reverse inputString
        startCoords = coordinatesParser startingString
        endCoords = coordinatesParser endingString

applyMove :: Board -> Move -> Board
applyMove (Board rows) (SingleMove ((y0,x0),(y1,x1))) = Board $ getNewBoard 0
    where
        getNewBoard :: Int -> [Row]
        getNewBoard rowNum 
            | rowNum >= 8 = []
            | rowNum /= y0 && rowNum /= y1 = (rows !! rowNum) : getNewBoard (rowNum + 1)
            | otherwise = getNewRow 0 rowNum : getNewBoard (rowNum + 1)
        getNewRow :: Int -> Int -> Row
        getNewRow squareNum rowNum
            | squareNum >= 8 = []
            | rowNum == y0 && squareNum == x0 = Empty : getNewRow (squareNum + 1) rowNum
            | rowNum == y1 && squareNum == x1 = getSquareFromCoordinates (Board rows) (y0,x0) : getNewRow (squareNum + 1) rowNum
            | otherwise = getSquareFromCoordinates (Board rows) (rowNum, squareNum) : getNewRow (squareNum + 1) rowNum

main :: IO()
main = putStr "\ESC[2J" >> gameLoop getDefaultStartingBoard Blue

gameLoop :: Board -> Color -> IO()
gameLoop board color = do
    putStrLn $ "Current turn: " ++ show color
    let currentValidMoves = [validMoves board (i,j) color | i <- [0..7], j <- [0..7]] >>= id
    -- print currentValidMoves
    print board
    putStr "Enter your move: "
    hFlush stdout
    moveInput <- getLine
    let parsedMove = parseMove moveInput
    if parsedMove `elem` currentValidMoves
    then putStr "\ESC[2J" >> gameLoop (applyMove board (parseMove moveInput)) (switchColor color)
    else putStrLn "\ESC[2J \60039 Invalid move" >> gameLoop board color

