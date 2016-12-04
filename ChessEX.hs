module ChessEX where
import Data.Char
import Data.List
import Data.Maybe

-- Datas and Types --
type Pos = (Int,Int) -- Pos
type Piece = (Rank, Pos) -- Piece

-------- BASIC FUNCTIONS ---------

-- Adds two positions together 
pPlus (x,y) (p,q)= (p+x,q+y) 

-- takeWhile but includes last elements
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []          =  []
takeWhile' p (x:xs)
    | p x       =  x : takeWhile' p xs
    | otherwise =  [x]

-- Map a condition for a list, ex map' (==10) [10,1,2,10] = [10,10] instead of [True,False,False,True]
map' :: (a -> Bool) -> [a] -> [a]
map' _ [] = []
map' p (x:xs) | p x = x : map' p xs
              | otherwise = map' p xs


data Color = Black | White -- Color
    deriving (Eq, Show)
data Rank = King | Queen | Knight | Rook | Bishop | Pawn -- Rank 
    deriving (Eq, Show)
data Board = Board ([Piece],[Piece]) -- Board, (whites,blacks)
    deriving (Eq,Show)

-- Create a new game -- 
newBoard :: Board
newBoard = Board (
    [ (Pawn,(i,2)) |i <- [1..8]] ++
    [ (Rook,(1,1)),  (Rook,(8,1)),  (Knight,(7,1))
    ,  (Knight,(2,1)), (Bishop,(3,1)),  (Bishop,(6,1))  -- Whites
    ,  (King,(5,1)) ,  (Queen,(4,1))]
    
    ,[ (Pawn,(i,7)) |i <- [1..8]] ++
    [ (Rook,(1,8)),  (Rook,(8,8)),  (Knight,(7,8))
    ,  (Knight,(2,8)),  (Bishop,(3,8)),  (Bishop,(6,8)) -- Blackes
    ,  (King,(5,8)) ,  (Queen,(4,8))])

-- Board Management --
addPiece :: (Piece,Color) -> Board -> Board
addPiece (p,White) (Board (w,b)) = Board (p:w,b)
addPiece (p,Black) (Board (w,b)) = Board (w,p:b)

getPiece :: Board -> Pos -> Maybe (Piece,Color)
getPiece (Board (white,black)) p = 
                    listToMaybe $ [((r,pos),Black)|(r,pos) <- black, pos == p] 
                     ++ [((r,pos),White)|(r,pos) <- white, pos == p]
           
removePiece :: Board -> Pos -> Board
removePiece (Board (w,b)) p 
                | getPiece (Board (w,b)) p == Nothing = (Board (w,b))
                | otherwise = Board ([(r,pos)| (r,pos) <- w, pos /= p]
                 ,[(r,pos)| (r,pos) <- b, pos /= p])
                 
changePiece :: Board -> Pos -> Rank -> Board
changePiece b p nRank = case getPiece b p of
                Just ((rank, p),c)  -> addPiece ((nRank,p),c) $ removePiece b p  
                _                   -> b   

-- Movement --  --------------- Not Done ----------------
move :: Board -> Pos -> Pos -> Board
move board from to | not (elem to (validMoves board from)) || 
                    (getPiece board from) == Nothing = board
                   | otherwise = 
                     addPiece ((rank,to),col) (removePiece board from) 
    where
        Just ((rank,_),col) = getPiece board from

-- Returns All the valid positions to move to of a Piece on a given position on the board.
validMoves :: Board -> Pos -> [Pos]
validMoves b p = [p' | p' <-(possibleMoves b p)
    , and[(onBoard p'), not(friendly b p p')]]

possibleMoves :: Board -> Pos -> [Pos]
possibleMoves b p = case getPiece b p of
     Just ((Pawn,(x,y)),White)  -> [(x+a,y+1)| a <- [-1..1]                     -- Pawn White -DONE-
        , ((a==0 && isEmptyPos b (x,y+1)) ||                                    
        (opponent b (x,y) (x+a,y+1) && a /= 0))] ++ [(x,y+2)|y == 2]
        
     Just ((Pawn,(x,y)),Black)  -> [(x+a,y-1)| a <- [-1..1]                     -- Pawn Black -DONE-
        , ((a==0 && isEmptyPos b (x,y-1)) || 
        (opponent b (x,y) (x+a,y-1) && a /= 0))] ++ [(x,y-2)|y == 7]          
        
     Just ((Rook,(x,y)),_)      -> clearCrossPath b (x,y)
        
     Just ((Bishop,(x,y)),_)    -> clearXPath b (x,y)  -- Bishop -Needs clearPath check
       
     Just ((Knight,(x,y)),_)    -> [(x+a,y+b)| a <- [-3,3]                      -- Knight -DONE-
        , b <- [-1,1]] ++ [(x+a,y+b)| a <- [-1,1], b <- [-3,3]]
        
     Just ((King,(x,y)),_)      -> [(a+x,b+y)|a <- [-1..1], b <- [-1..1]]       -- King -DONE- -needs Checkmate
        
     Just ((Queen,(x,y)),_)     -> clearCrossPath b (x,y) ++ clearXPath b (x,y)
        
     _                          -> []                                           -- Empty position
     

 -- Bishops
clearXPath b (x,y) =  map' (oppOrEmpty b (x,y)) $ takeWhile'(isEmptyPos b)[(x+i,y+i)|i <- [1..7]]
    ++ takeWhile'(isEmptyPos b)[(x+i,y-i)|i <- [1..7]]++ takeWhile(isEmptyPos b)[(x-i,y+i)|i <- [1..7]]
    ++ takeWhile'(isEmptyPos b)[(x-i,y-i)|i <- [1..7]]
        

-- Rooks and Queens
clearCrossPath b (x,y) = map' (oppOrEmpty b (x,y)) $ takeWhile'(isEmptyPos b)[(x+i,y)|i <- [8-x..8]]
    ++ takeWhile'(isEmptyPos b)[(x-i,y)|i <- [1..8-x]]++ takeWhile(isEmptyPos b)[(x,y+i)|i <- [8-y..8]]
    ++ takeWhile'(isEmptyPos b)[(x,y-i)|i <- [1..8-y]]
        
isEmptyPos :: Board -> Pos -> Bool
isEmptyPos b p = getPiece b p == Nothing
                  
onBoard :: Pos -> Bool
onBoard (x,y) = and [x >= 1, x <= 8, y >= 1, y <= 8]

friendly :: Board -> Pos -> Pos -> Bool
friendly b p1 p2 | getPiece b p1 == Nothing || getPiece b p2 == Nothing = False
                 | otherwise = c1 == c2
    where 
        Just (_,c1) = getPiece b p1
        Just (_,c2) = getPiece b p2
        
opponent :: Board -> Pos -> Pos -> Bool
opponent b p1 p2 = not $ or[friendly b p1 p2, isEmptyPos b p2] 

oppOrEmpty :: Board -> Pos -> Pos -> Bool
oppOrEmpty b p1 p2 = (opponent b p1 p2) || (isEmptyPos b p2)
          
-- Output Input --
printBoard :: Board -> IO()
printBoard = putStrLn . showBoard

showBoard :: Board -> String
showBoard b = "\n \tA\tB\tC\tD\tE\tF\tG\tH\n" ++ 
    (concat $ concat $ [[ "    "++(show (8-y)) ++ "  "] ++ 
    [(fst (showPiece b (x,8-y)))++"\t"|x <- [1..8]]++["\n"]| y <- [0..7]])
            
showPiece :: Board -> Pos -> (String,String)
showPiece b p = case getPiece b p of
                    Just ((r,(x,y)),c)  ->  (show r, show c ++ " " ++ show r)
                    _               -> ("X","No Piece")
                       
-- Not finnished --
readBoard :: FilePath -> IO Board 
readBoard path = do
            file <- readFile path
            return newBoard
            
saveBoard :: Board -> IO ()
saveBoard = undefined
            
