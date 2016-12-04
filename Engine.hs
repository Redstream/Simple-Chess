module Engine where

import AI as AI
import Chess
import Data.Char

-- Starts a new game -- 
main = go newBoard White

-- Main gameloop --
go b turn = do
        printBoard b
        if(gameOver b || stalemate b)then
            error ("Gameover, winner is :" ++ show(winner b))
        else
            do
            putStr(show(turn)++"'s move: ")
            input <- getLine
            let cmd = inputString $  map toLower input
            case cmd of                                         -- Command Handler --    
                ((0,0):_,_)         ->  error("Exit")             
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
                ((0,3):_,"black")   ->  putStrLn $ concat $ map showPos $ allMoves b Black          
                ((0,3):_,"white")   ->  putStrLn $ concat $ map showPos $ allMoves b White          
                ((0,3):_,c)         ->  putStrLn ("\nInvalid argument: "++ c)
                ((0,4):_,c)         ->  putStrLn ("\ncmds:\n" ++ c)
                ((1,x):_,a)         ->  putStrLn ("\nError "++show x++": "++ a)
            
            go b turn
    where      
    go' b p1 p2 = move b p1 p2    
    turn'   | turn == White = Black
            | otherwise = White

-- Converts the input to the different commands readable by the gameloop       
inputString :: String -> ([(Int,Int)],String)
inputString s = case raw_input s of
                ("quit":_)          -> ([(0,0)],"")
                ("move":p1:p2:_)    -> ([(0,1),convert p1, convert p2],"")
                ("moves":p:_)       -> ([(0,2),convert p],"")
                ("allmoves":c:_)    -> ([(0,3)],c)
                ("ls":_)            -> ([(0,4)]," quit\n move p1 p2\n moves p1\n allmoves c\n ls")
                a                   -> ([(1,0)],unwords a)
    where
    raw_input = words 
    convert (x:y:_) = ((ord x) - 96,(ord y)-48)
    convert _       = (-1,-1)
    
    
-- IO --
printBoard :: Board -> IO()
printBoard = putStrLn . showBoard

showBoard :: Board -> String
showBoard b = "\n\tA\tB\tC\tD\tE\tF\tG\tH\n" ++ 
    (concat $ concat $ [[ "    "++(show (8-y)) ++ "  "] ++ 
    [(snd (showPiece b (x,8-y)))++"\t"|x <- [1..8]]++["\n"]| y <- [0..7]])
            
showPiece :: Board -> Pos -> (String,String)
showPiece b p = case getPiece b p of
                    Just ((r,(x,y)),c)  ->  (show r,[head (show c)]++ " "++ take 2 (show r)) -- ++[head (show c)] ,show c ++ " " ++ show r
                    
                    _               -> ("X","X")
                    
showPos :: Pos -> String
showPos (x,y) = "(" ++[chr (x+64)] ++ "," ++ show y ++ ") "
                      
