module ValidMoves where

import Board

clearIf :: [t] -> Bool -> [t]
clearIf _ True = [] 
clearIf list False = list 

fromOriginMove :: Coordinates -> Coordinates -> Move
fromOriginMove origin coordinates = SingleMove (origin, coordinates)

validMoves :: Board -> Coordinates -> Color -> [Move]
validMoves board coordinates color
    | isEmpty square = []
    | Piece pieceType pieceColor <- square, pieceColor /= color = []
    | Piece pieceType pieceColor <- square, pieceType == Pawn && pieceColor == Blue = bluePawnMoves
    | otherwise = []
    where
        square = getSquareFromCoordinates board coordinates
        (y,x) = coordinates

        isEmptyFromCoords :: Coordinates -> Bool
        isEmptyFromCoords coordinates = isEmpty $ getSquareFromCoordinates board coordinates

        bluePawnTargets = clearIf [(y-1,x)] (not $ isEmptyFromCoords (y-1,x)) ++ 
                          clearIf [(y-2,x)] (not (isEmptyFromCoords (y-1,x)) && not (isEmptyFromCoords (y-2,x))) ++ 
                          clearIf [(y-1,x+1)] (x == 7 || isEmptyFromCoords (y-1,x+1) || (getColorFromCoordinates board (y-1,x+1) == Blue)) ++ 
                          clearIf [(y-1,x-1)] (x == 0 || isEmptyFromCoords (y-1,x-1) || (getColorFromCoordinates board (y-1,x-1) == Blue)) 
        bluePawnMoves = fromOriginMove coordinates <$> bluePawnTargets
