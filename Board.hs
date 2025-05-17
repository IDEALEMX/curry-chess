module Board where

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
                | otherwise = showColorAnsi (color piece) ++ showPiece (pieceType piece) ++ " " ++ loopSquares column rowNum (squareNum + 1)
                where
                    piece = column !! squareNum

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

-- a move represent the movement from the piece in the first coordinate
-- to the place of the second coordinate. with the exeption of castling
data Move = SingleMove ((Int, Int), (Int, Int)) | ShortCastle | LongCastle
