module Engine where

import Chess
import Data.Char

-- Starts a new game --
main :: IO()
main = do
        clearTerminal
        go newBoard White

-- Main gameloop --
go :: Board -> Color -> IO()
go b turn = do
        printBoard b
        if(gameOver b || stalemate b) then error ("Gameover, winner is :" ++ show(winner b))
        else
            do
            putStr(show(turn) ++ "'s turn: ")
            input <- getLine
            clearTerminal
            let cmd = inputString $  map toLower input
            case cmd of
                ((0,0):_,_)         ->  error("Exiting")
                ((0,1):p1:p2:_,_ )  ->  do
                                        let b' = go' b p1 p2
                                        if b' == b then putStrLn("Invalid move")
                                        else 
                                            if(c == turn)
                                            then go b' turn'
                                            else putStrLn("Not a " ++ show turn ++ " piece.")
                                            where 
                                                Just (piece,c) = getPiece b (p1)
                ((0,2):p1:_,_)      ->  putStrLn $ concat $ map showPos $ validMoves b p1    
                ((0,3):_,c)         ->  putStrLn $ concat $ map showPos $ allMoves b $ (\s -> case s of "black" -> Black
                                                                                                        _       -> White) c
                ((0,4):_,c)         ->  putStrLn ("\nAvailable commands:\n" ++ c)
                ((1,x):_,a)         ->  putStrLn ("\nUnrecognized command: " ++ a)
            go b turn -- Go back to start
    where      
        go' b p1 p2 = move b p1 p2
        turn'   | turn == White = Black
                | otherwise = White

-- Converts the input to the different commands readable by the gameloop       
inputString :: String -> ([(Int,Int)],String)
inputString s = case words s of
                ("quit":_)          -> ([(0,0)],"")
                ("move":p1:p2:_)    -> ([(0,1),convert p1, convert p2],"")
                ("moves":p:_)       -> ([(0,2),convert p],"")
                ("allmoves":c:_)    -> ([(0,3)],c)
                ("help":_)          -> ([(0,4)]," quit\n move from to\n moves pos\n allmoves color\n help")
                a                   -> ([(1,0)],unwords a)
    where
        convert (x:y:_) = ((ord x) - 96,(ord y)-48)
        convert _       = (-1,-1)
    

printBoard :: Board -> IO()
printBoard = putStrLn . showBoard

clearTerminal :: IO()
clearTerminal = putStrLn $ concat["\n"| x <-[1..30]]

showBoard :: Board -> String
showBoard b = "\n\t   A   B   C   D   E   F   G   H\n" ++
    (concat $ concat $ [[ "\t" ++ (show (8-y)) ++ "  "] ++
    [(showPiece b (x,8-y)) ++ "   "|x <- [1..8]] ++ ["\n"]| y <- [0..7]])
            
showPiece :: Board -> Pos -> [Char]
showPiece b p = case getPiece b p of
                    Just ((r,(x,y)),c)  -> [chr (9812 + colorOffset c + rankOffset r)]
                    _                   -> ['\9634']
    where
        colorOffset White   = 6
        colorOffset _       = 0
        rankOffset King     = 0
        rankOffset Queen    = 1
        rankOffset Rook     = 2
        rankOffset Bishop   = 3
        rankOffset Knight   = 4
        rankOffset Pawn     = 5
                    
showPos :: Pos -> String
showPos (x,y) = "(" ++[chr (x+64)] ++ "," ++ show y ++ ") "
                      
