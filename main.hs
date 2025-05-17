-- chess data types
data PieceType = Pawn | Rook | Bishop | Knight | King | Queen
data Color = Red | Blue
data Square = Empty | Piece { pieceType :: PieceType, color :: Color }

-- Check if a given square is empty
isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

type Column = [Square]
newtype Board = Board [Column]

instance Show Board where

    show :: Board -> String
    show (Board columns) = loopRows columns 0
        where
            loopRows :: [Column] -> Int -> String
            loopRows columns rowNum 
                | rowNum > 7 = ""
                | otherwise =  loopSquares (columns !! rowNum) rowNum 0 ++ "\n" ++ loopRows columns (rowNum + 1)
            
            loopSquares :: Column -> Int -> Int -> String
            loopSquares column rowNum squareNum
                | squareNum > 7 = ""
                | isEmpty $ column !! squareNum = showEmpty rowNum squareNum ++ " " ++ loopSquares column rowNum (squareNum + 1)
                | otherwise = "b" ++ " " ++ loopSquares column rowNum (squareNum + 1)
                where
                    piece = column !! squareNum

            showEmpty :: Int -> Int -> String
            showEmpty rowNum squareNum
                --  scape code
                | even (rowNum + squareNum) = "\61640" 
                --  scape code
                | otherwise = "\61590"

            -- Need to add pieces with their respective color

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

main :: IO()
main = print getDefaultStartingBoard
