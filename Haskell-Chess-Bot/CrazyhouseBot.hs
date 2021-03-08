
module CrazyhouseBot
    ( getMove
    , listMoves
    ) 
    where

import Data.Char
import Util
import System.IO
import Data.Char
import Data.List
import Data.Maybe

getMove :: String -> String
getMove state = head (filteredMoves (replace2(state)))


listMoves :: String -> String
listMoves state = "[" ++ intercalate "," (filteredMoves (replace2(state))) ++ "]"


blackfigures = ['k','q','b','n','r','p']
whitefigures = ['K','Q','B','N','R','P']
allfigures = ['k','q','b','n','r','p', 'K','Q','B','N','R','P']
xaxis = ['a', 'b', 'c', 'd' ,'e', 'f', 'g', 'h', 'i'] --i for debugging purposes

linebreaks = [8,17,26,35,44,53,62,71]
white = 1
black = -1

getFieldnr ::  Int -> Int -> Int
getFieldnr x y = (9*y) + x

getField :: String -> Int -> Char
getField (x:xs) 0 = x
getField (x:xs) c = getField xs (c-1)

getFieldx :: Int -> Int
                -- x  ,   y               
getFieldx c = c `mod` 9

getFieldy :: Int -> Int
                -- x  ,   y               
getFieldy c = c `div` 9

convertMoveToBoard:: String -> String -> String
convertMoveToBoard state move = state2
                                where   coord_old 
                                                | length move == 5 = invConvertPosition (take 2 move)
                                                | length move == 4 = (-1,-1)
                                        coord_new 
                                                | length move == 5 = invConvertPosition (drop 3 move)
                                                | length move == 4 = invConvertPosition (drop 2 move) 
                                        oldx 
                                                | length move == 5 = fst coord_old
                                                | length move == 4 = -1
                                        oldy
                                                | length move == 5 = snd coord_old
                                                | length move == 4 = -1
                                        newx = fst coord_new
                                        newy = snd coord_new
                                        
                                        state1
                                                | length move == 5 = changeFigure state oldx oldy '1' 
                                                | length move == 4 = state

                                        figure 
                                                | length move == 5 = getField state (getFieldnr oldx oldy)
                                                | length move == 4 = (head move)
                                        state2 = changeFigure state1 newx newy figure


kingDeadWhite:: String -> Bool
kingDeadWhite state = iterateOverBoard state 'K' == []

kingDeadBlack:: String -> Bool
kingDeadBlack state = iterateOverBoard state 'k' == []

postMoveCheck:: String -> String -> Bool
postMoveCheck state move 
                                | last state == 'b' = any (\enemyMove -> kingDeadBlack (convertMoveToBoard changedBoard enemyMove)) enemyMoves
                                | last state == 'w' = any (\enemyMove -> kingDeadWhite (convertMoveToBoard changedBoard enemyMove)) enemyMoves
                                | otherwise = True
                                        where   changedBoard = convertMoveToBoard (changePlayer state) move --spielbrett nach move und mit invertiertem player
                                                enemyMoves = getAllMoves changedBoard



changePlayer:: String -> String
changePlayer state 
                        |last state == 'b' = (init state)++ ['w']
                        |last state == 'w' = (init state)++ ['b']

filterCheckMoves:: String -> [String] -> [String] --(filter (/= c) cs)
filterCheckMoves state moves = filter (\move -> not(postMoveCheck state move)) moves

isEnemy :: Char -> Char -> Bool
isEnemy source destination 
                        |any (==source) whitefigures = any (==destination) blackfigures
                        |any (==source) blackfigures = any (==destination) whitefigures
                        |otherwise = False


iterateOverBoard :: String -> Char -> [Int]
iterateOverBoard state2 figure = iterateOverString state2 figure 0

iterateOverString :: String -> Char -> Int -> [Int]
iterateOverString state2 figure 72 = []
iterateOverString state2 figure index = if (getField state2 index == figure) then iterateOverString state2 figure (index+1) ++ [index] else iterateOverString state2 figure (index+1)


rightColor :: String -> Int
rightColor board
                        |last board == 'w'  = 1
                        |last board == 'b'  = -1

replace2::String->String
replace2 x = concat(map repl x)
        where repl x | isDigit x && xi > 1 = replicate xi '1'
                where   xi = digitToInt x
              repl x = [x]

outOfBounds :: Int -> Int -> Bool
outOfBounds x y = 
                let c = getFieldnr x y 
                in c>71 || c<0 || any (==c) linebreaks


changeFigure:: String -> Int -> Int -> Char -> String
changeFigure state x y mychar = 
                            let c = getFieldnr x y
                            in take (c) state ++ [mychar] ++ drop (c+1) state



getFigurePosition:: String -> Char -> Char -> [Int]
getFigurePosition state whiteFigure blackFigure 
                                                |last state == 'w' = iterateOverBoard state whiteFigure 
                                                |otherwise = iterateOverBoard state blackFigure

getEmptyFields:: String -> [Int]
getEmptyFields state = iterateOverBoard state '1'

getReserveMoves:: [Int] -> Char -> [String]
getReserveMoves [] _ = []
getReserveMoves (c:cs) mychar   
                                | (mychar == 'p' || mychar == 'P') && (getFieldy c == 0 || getFieldy c == 7) = [] ++ (getReserveMoves cs mychar) 
                                | otherwise = (getReserveMoves cs mychar) ++ [[mychar] ++ "-" ++ (convertPosition (getFieldx c) (getFieldy c))] 



reserveCombined:: String -> Char -> [String]
reserveCombined state figure = getReserveMoves (getEmptyFields state) figure



allReserveMoves:: String -> String -> [String]
allReserveMoves [] state = []
allReserveMoves (c:cs) state 
                                | (last state) == 'w' && c `elem` whitefigures =  (allReserveMoves (filter (/= c) cs) state) ++ reserveCombined (state) c
                                | (last state) == 'b' && c `elem` blackfigures =  (allReserveMoves (filter (/= c) cs) state) ++ reserveCombined (state) c
                                | otherwise = allReserveMoves cs state

allReserveMovesHelper:: String -> [String]
allReserveMovesHelper state = allReserveMoves (drop 72 (init(init(state)))) state

                        
filteredMoves:: String -> [String]
filteredMoves state = filterCheckMoves state (getAllMoves state)

getAllMoves:: String -> [String]
getAllMoves state = kings ++ queens ++ bishops ++ knights ++ rooks ++ pawns ++ reserve
                        where   kings = getAllFigureMoves state (getFigurePosition state 'K' 'k') 'K'
                                queens = getAllFigureMoves state (getFigurePosition state 'Q' 'q') 'Q'
                                bishops = getAllFigureMoves state (getFigurePosition state 'B' 'b') 'B'
                                knights = getAllFigureMoves state (getFigurePosition state 'N' 'n') 'N'
                                rooks = getAllFigureMoves state (getFigurePosition state 'R' 'r') 'R'
                                pawns = getAllFigureMoves state (getFigurePosition state 'P' 'p') 'P'
                                reserve = allReserveMovesHelper state
--Neu
getAllFigureMoves::String-> [Int]-> Char -> [String]
getAllFigureMoves state [] _ = []             --['K','Q','B','N','R','P']                                      
getAllFigureMoves state (c:cs) 'K' = getAllFigureMoves state cs 'K' ++ (movesKingAll state (getFieldx c) (getFieldy c))
getAllFigureMoves state (c:cs) 'Q' = getAllFigureMoves state cs 'Q' ++ (movesQueen state (getFieldx c) (getFieldy c))
getAllFigureMoves state (c:cs) 'B' = getAllFigureMoves state cs 'B' ++ (movesBishop state (getFieldx c) (getFieldy c))
getAllFigureMoves state (c:cs) 'N' = getAllFigureMoves state cs 'N' ++ (movesKnightAll state (getFieldx c) (getFieldy c))
getAllFigureMoves state (c:cs) 'R' = getAllFigureMoves state cs 'R' ++ (movesRook state (getFieldx c) (getFieldy c))
getAllFigureMoves state (c:cs) 'P' = getAllPawnMovesHelper state

getAllPawnMovesHelper :: String -> [String]
getAllPawnMovesHelper state = getAllPawnMoves state (getFigurePosition state 'P' 'p') color
                        where color = rightColor state


               --Bekommt state, Liste mit Positionen (im String!) und color als -1 oder 1
               --Gibt eine Liste aller formatierten Moves zurueck
getAllPawnMoves::String-> [Int]-> Int-> [String]
getAllPawnMoves state [] color = []                                                   
getAllPawnMoves state (x:xs) color = getAllPawnMoves state xs color ++ (movesPawn state (getFieldx x) (getFieldy x) color )

movesPawn:: String-> Int-> Int-> Int-> [String]
movesPawn state x y color = (movesPawnUpOnce state x y color) ++ (movesPawnUpLeft state x y color) ++ (movesPawnUpRight state x y color) ++ (movesPawnUpTwice state x y color)

convertPosition :: Int -> Int -> String
convertPosition x y = [xaxis!!x] ++ (show (8-y))

invConvertPosition:: String -> (Int, Int)
invConvertPosition coordinate = (x,y)
                                where   x = fromMaybe 0 (elemIndex (head coordinate) xaxis)
                                        y = 8 - (read [last coordinate])

movesPawnUpOnce :: String-> Int-> Int-> Int -> [String]
movesPawnUpOnce state x y color --color ist 1 fuer weiß und -1 fuer schwarz
                        -- destination may not be out of bounds and must be empty ; also first set destination to thisFigure und the current position to 1
                        | (not(outOfBounds x (y-(1*color)))) && destination == '1' = [(convertPosition x y) ++ "-" ++ (convertPosition x (y-(1*color)))] 
                        | otherwise = []
                        where   c = getFieldnr x y 
                                thisFigure = getField state c
                                d = getFieldnr x (y-(1*color))
                                destination = getField state d

movesPawnUpLeft :: String-> Int-> Int-> Int ->[String]
movesPawnUpLeft state x y color
                        -- destination may not be out of bounds and must be an Enemy ; also first set destination to thisFigure und the current position to 1
                        |  not(outOfBounds (x-1) (y-(1*color))) && (isEnemy thisFigure destination) = [(convertPosition x y) ++ "-" ++ (convertPosition (x-1) (y-(1*color)))]
                        | otherwise = []
                    where   c = getFieldnr x y 
                            thisFigure = getField state c
                            d = getFieldnr (x-1) (y-(1*color))
                            destination = getField state d

movesPawnUpRight :: String-> Int-> Int-> Int ->[String]
movesPawnUpRight state x y color
                        -- destination may not be out of bounds and must be an Enemy ; also first set destination to thisFigure und the current position to 1
                        | not(outOfBounds (x+1) (y-(1*color))) && (isEnemy thisFigure destination) = [(convertPosition x y) ++ "-" ++ (convertPosition (x+1) (y-(1*color)))]
                        | otherwise = []
                    where   c = getFieldnr x y 
                            thisFigure = getField state c 
                            d = getFieldnr (x+1) (y-(1*color))
                            destination = getField state d

movesPawnUpTwice :: String-> Int-> Int-> Int ->[String]
movesPawnUpTwice state x y color
                        -- destination may not be out of bounds and must be empty ; also first set destination to thisFigure und the current position to 1
                        | y==((7 + (5*color)) `div` 2) && destination == '1' && fieldbeforeD == '1' = [(convertPosition x y) ++ "-" ++ (convertPosition x (y-(2*color)))]
                        | otherwise = []                   --y == 6 fuer weis und y==1 fuer schwarz     
                    where   c = getFieldnr x y 
                            thisFigure = getField state c
                            d = getFieldnr x (y-(2*color))
                            destination = getField state d
                            e = getFieldnr x (y-(1*color))
                            fieldbeforeD = getField state e
                            

movesRec::String -> Int -> Int -> Int -> Int -> Int -> Int->[String]
movesRec state original_x original_y x y dirx diry 
                        | not(outOfBounds (x+dirx) (y+diry)) && destination == '1' = [firstfield ++ "-" ++ (convertPosition (x+dirx) (y+diry))] ++ movesRec (changeFigure (changeFigure state x y '1') (x+dirx) (y+diry) thisFigure) original_x original_y (x+dirx) (y+diry) dirx diry
                        | not(outOfBounds (x+dirx) (y+diry)) && (isEnemy thisFigure destination) = [firstfield ++ "-" ++ (convertPosition (x+dirx) (y+diry))]
                        | otherwise = []
                        where   c = getFieldnr x y 
                                thisFigure = getField state c
                                d = getFieldnr (x+dirx) (y+diry)
                                destination = getField state d
                                firstfield = (convertPosition (original_x) (original_y))
                        

movesQueen:: String->Int->Int-> [String]
movesQueen state x y = up ++ upright ++ right ++ downright ++ down ++ downleft ++ left ++ upleft
                        where   up = movesRec state x y x y 0 (-1)
                                upright = movesRec state x y x y 1 (-1)
                                right = movesRec state x y x y 1 0
                                downright = movesRec state x y x y 1 1
                                down = movesRec state x y x y 0 1
                                downleft = movesRec state x y x y (-1) 1
                                left = movesRec state x y x y (-1) 0
                                upleft = movesRec state x y x y (-1) (-1)

movesBishop:: String->Int->Int-> [String]
movesBishop state x y = upright ++ downright ++ downleft ++ upleft
                        where   upright = movesRec state x y x y 1 (-1)
                                downright = movesRec state x y x y 1 1
                                downleft = movesRec state x y x y (-1) 1
                                upleft = movesRec state x y x y (-1) (-1)

movesRook :: String->Int->Int-> [String]
movesRook state x y = up ++ right ++ down ++ left
                        where   up = movesRec state x y x y 0 (-1)
                                right = movesRec state x y x y 1 0
                                down = movesRec state x y x y 0 1
                                left = movesRec state x y x y (-1) 0

movesKnightAll::String->Int->Int-> [String]
movesKnightAll state x y 
                        | (x==0) = upUpRight ++ upRightRight ++ downRightRight ++ downDownRight
                        | (x==7) = downDownLeft ++ downLeftLeft ++ upLeftLeft ++ upUpLeft
                        |otherwise = upUpRight ++ upRightRight ++ downRightRight ++ downDownRight++ downDownLeft++ downLeftLeft ++ upLeftLeft ++ upUpLeft
                                where   upUpRight = moveKnight state x y (1) (-2) 
                                        upRightRight = moveKnight state x y (2) (-1) 
                                        downRightRight = moveKnight state x y (2) (1) 
                                        downDownRight = moveKnight state x y (1) (2) 
                                        downDownLeft = moveKnight state x y (-1) (2) 
                                        downLeftLeft = moveKnight state x y (-2) (1) 
                                        upLeftLeft = moveKnight state x y (-2) (-1) 
                                        upUpLeft = moveKnight state x y (-1) (-2)



moveKnight::String->Int->Int->Int->Int-> [String]
moveKnight state x y dirx diry 
                                | (outOfBounds (x+dirx) (y+diry)) = []
                                | (isEnemy thisFigure destination) || (destination == '1')  = [(convertPosition x y) ++ "-" ++ (convertPosition (x+dirx) (y+diry))]
                                |otherwise = []
                                 where  c = getFieldnr x y 
                                        thisFigure = getField state c
                                        d = getFieldnr (x+dirx) (y+diry)
                                        destination = getField state d

movesKingAll:: String-> Int-> Int->[String]
movesKingAll state x y = up ++ upright ++ right ++ downright ++ down ++ downleft ++ left ++ upleft
                        where   up = movesKing state x y 0 (-1)
                                upright = movesKing state x y 1 (-1)
                                right = movesKing state x y 1 0
                                downright = movesKing state x y 1 1
                                down = movesKing state x y 0 1
                                downleft = movesKing state x y (-1) 1
                                left = movesKing state x y (-1) 0
                                upleft = movesKing state x y (-1) (-1)

movesKing:: String-> Int-> Int-> Int-> Int-> [String]
movesKing state x y dirx diry 
                        -- destination may not be out of bounds and must be empty ; also first set destination to thisFigure und the current position to 1
                        | outOfBounds (x+dirx) (y+diry) = []
                        | (isEnemy thisFigure destination) || (destination == '1') = [(convertPosition x y) ++ "-" ++ (convertPosition (x+dirx) (y+diry))]
                        | otherwise = []
                        where   c = getFieldnr x y 
                                thisFigure = getField state c
                                d = getFieldnr (x+dirx) (y+diry)
                                destination = getField state d

