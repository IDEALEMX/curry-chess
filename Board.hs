module Board where

import Data.Char (ord)

-- chess data types
data PieceType = Pawn | Rook | Bishop | Knight | King | Queen deriving Eq
data Color = Red | Blue deriving Eq

switchColor :: Color -> Color
switchColor Red = Blue
switchColor Blue = Red

data Square = Empty | Piece { pieceType :: PieceType, color :: Color }

-- Check if a given square is empty
isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False
type Row = [Square]
newtype Board = Board [Row]

instance Show Board where

    show :: Board -> String
    show (Board rows) = loopRows rows 0
        where
            loopRows :: [Row] -> Int -> String
            loopRows rows rowNum 
                | rowNum > 7 = "a b c d e f g h"
                | otherwise =  loopSquares (rows !! rowNum) rowNum 0 ++ "\ESC[0;37m" ++ show (reverse [1..8] !! rowNum) ++ "\n" ++ loopRows rows (rowNum + 1)
            
            loopSquares :: Row -> Int -> Int -> String
            loopSquares row rowNum squareNum
                | squareNum > 7 = ""
                | isEmpty $ row !! squareNum = showEmpty rowNum squareNum ++ " " ++ loopSquares row rowNum (squareNum + 1)
                | otherwise = showColorAnsi (color piece) ++ showPiece (pieceType piece) ++ " " ++ loopSquares row rowNum (squareNum + 1)
                where
                    piece = row !! squareNum

            showEmpty :: Int -> Int -> String
            showEmpty rowNum squareNum
                | even (rowNum + squareNum) = "\ESC[0;37m\61640" --  scape code
                | otherwise = "\ESC[0;37m\61590" --  scape code

            -- Need to add pieces with their respective color
            
            showColorAnsi :: Color -> String
            showColorAnsi Red = "\ESC[0;31m"
            showColorAnsi Blue = "\ESC[0;34m"

            showPiece :: PieceType -> String
            showPiece Rook = "\60774" --  scape code
            showPiece Knight = "\60771" --  scape code
            showPiece Bishop = "\60768" --  scape code
            showPiece Queen  = "\60773" --  scape code
            showPiece King = "\60770" --  scape code
            showPiece Pawn = "\60772" --  scape code

getSquareFromCoordinates :: Board -> Coordinates -> Square
getSquareFromCoordinates (Board rows) (y, x) = (rows !! y) !! x

-- a move represent the movement from the piece in the first coordinate
-- to the place of the second coordinate. with the exeption of castling

-- coordinates follow the (y, x) structure for ease of use even if non standard
type Coordinates = (Int, Int)
data Move = SingleMove (Coordinates, Coordinates) | ShortCastle | LongCastle deriving Show
