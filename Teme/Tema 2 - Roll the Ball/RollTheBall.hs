{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.List
import Data.Array as A
{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}

type Game_Matrix = (A.Array (Int, Int) Cell)

data Cell = Cell {cell_data :: [Char]} deriving (Eq, Ord)
 
{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level {level_state :: Game_Matrix} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

getCoordinates :: Position -> Position
getCoordinates pos = (fst pos, snd pos)

buildArray :: Position -> a -> (A.Array (Int, Int) a)
buildArray size value = array ((0,0), (fst size, snd size)) [((x,y), value) | x <- [0..fst size], y <- [0..snd size]]

allPermutations :: Game_Matrix -> [Position]
allPermutations game = (range((0,0), lowerRightCorner(game)))

updateElement :: Game_Matrix -> Cell -> Position -> Game_Matrix
updateElement arr element pos = arr A.//[((fst pos, snd pos), element)]

lowerRightCorner :: Game_Matrix -> Position
lowerRightCorner = snd . A.bounds

lowerLeftCorner :: Game_Matrix -> Position
lowerLeftCorner = fst . A.bounds

elemAt :: Game_Matrix -> Position-> Cell
elemAt arr pos = arr A.! pos

updateCell :: [Char] -> Cell
updateCell c = Cell (c)

addEndl :: Game_Matrix -> Game_Matrix
addEndl arr = arr A.// [((i, snd $ lowerLeftCorner arr), updateCell ("\n" ++ (cell_data $ elemAt arr (i, snd $ lowerLeftCorner arr)))) | i <- [0..fst (lowerRightCorner arr)]]

elemToList :: a -> [a]
elemToList a = [a]

firstElem :: [a] -> a
firstElem x = head x

cellToChar :: Cell -> Char
cellToChar a = firstElem $ cell_data a
{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Level 
    where show givenLevel = intercalate "" (map (\s -> cell_data s) $ A.elems (addEndl(level_state givenLevel))) ++ "\n"

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

constructLevel :: Game_Matrix -> Level
constructLevel arr = Level (arr)

emptyLevel :: Position -> Level
emptyLevel givenPosition = constructLevel (buildArray givenPosition $ updateCell $ elemToList (emptySpace))

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCellIfEmpty :: Position -> Game_Matrix -> Cell -> Game_Matrix
addCellIfEmpty pos arr element = arr A.//[((fst pos, snd pos), element)]


addCell :: (Char, Position) -> Level -> Level
addCell pos givenLevel 
        | fst(p) < 0 || snd(p) < 0 = givenLevel
        | fst(p) > fst(q) || snd(p) > snd(q) = givenLevel 
        | otherwise = Level ((addCellIfEmpty p game_state (updateCell (elemToList $ (fst pos)))))
            where {
                game_state = level_state(givenLevel);
                q = lowerRightCorner(game_state);
                p = snd(pos)
            }

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos coordList = foldr addCell (emptyLevel pos) coordList


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

checkIfGivenType :: Game_Matrix-> Position -> [Char] -> Bool
checkIfGivenType game pos cList = (elem (cellToChar (elemAt game pos)) cList)

isValidMove :: Game_Matrix ->Position -> Position -> Bool
isValidMove game givenPosition nextPosition
    | (x1 + x2 < xout1) || (x1 + x2 > xout2) || (y1 + y2 < yout1) || (y1 + y2 > yout2) = False
    | (firstElem (cell_data (elemAt game (x1 + x2, y1 + y2)))) /= emptySpace = False
    | (checkIfGivenType game givenPosition winningCells) == True || (checkIfGivenType game givenPosition startCells) == True = False
    | otherwise = True
        where {
            x1 = fst givenPosition;
            x2 = fst nextPosition;
            y1 = snd givenPosition;
            y2 = snd nextPosition;
            xout1 = fst $ lowerLeftCorner(game);
            xout2 = fst $ lowerRightCorner(game);
            yout1 = snd $ lowerLeftCorner(game);
            yout2 = snd $ lowerRightCorner(game);
        }
    
moveCell :: Position -> Directions -> Level -> Level
moveCell pos dir givenLevel
    | dir == South && isValidMove (level_state(givenLevel)) pos (1, 0) = (addCell (emptySpace, pos) (addCell (current_cell, new_pos_south) givenLevel))
    | dir == North && isValidMove (level_state(givenLevel)) pos (-1, 0) = (addCell (emptySpace, pos) (addCell (current_cell, new_pos_north) givenLevel))
    | dir == East && isValidMove (level_state(givenLevel)) pos (0, 1) = (addCell (emptySpace, pos) (addCell (current_cell, new_pos_east) givenLevel))
    | dir == West && isValidMove (level_state(givenLevel))pos (0, -1) = (addCell (emptySpace, pos) (addCell (current_cell, new_pos_west) givenLevel))
    | otherwise = givenLevel

    where {
        current_cell = (firstElem(cell_data (elemAt (level_state(givenLevel)) pos)));
        new_pos_south = (fst pos + 1, snd pos);
        new_pos_north = (fst pos - 1, snd pos);
        new_pos_east = (fst pos, snd pos + 1);
        new_pos_west = (fst pos, snd pos - 1);
    }

{-
    *** TODO ***

    Verifică dacă două celule se pot conecta.
    Atenție: Această funcție se aplică de la stânga la 
    dreapta(nu este comutativă).

    ex: connection botLeft horPipe = True (╚═)
        connection horPipe botLeft = False (═╚)
-}

connectionCells :: [[Char]]
connectionCells = [[horPipe, botLeft, topLeft, winRight], -- West
                   [horPipe, botRight, topRight, winLeft], -- East
                   [verPipe, topRight, topLeft, winDown], -- North
                   [verPipe, botLeft, botRight, winUp]] -- South

checkIfGivenTypeCell :: Cell -> [Char] -> Bool
checkIfGivenTypeCell c cList = (elem (cellToChar (c)) cList)

connection :: Cell -> Cell -> Directions -> Bool
connection firstCell secondCell dir
    | (fc [horPipe]) && (dir == West && (sc (connectionCells !! 0)) || (dir == East && (sc (connectionCells !! 1)))) = True
    | (fc [verPipe]) && (dir == North && (sc (connectionCells !! 2)) || (dir == South && (sc (connectionCells !! 3)))) = True
    | (fc [topLeft]) && (dir == South && (sc (connectionCells !! 3)) || (dir == East && (sc (connectionCells !! 1)))) = True
    | (fc [botLeft]) && (dir == East && (sc (connectionCells !! 1)) || (dir == North && (sc (connectionCells !! 2)))) = True
    | (fc [botRight]) && (dir == West && (sc (connectionCells !! 0)) || (dir == North && (sc (connectionCells !! 2)))) = True
    | (fc [topRight]) && (dir == South && (sc (connectionCells !! 3)) || (dir == West && (sc (connectionCells !! 0)))) = True
    | (fc [startUp]) && (dir == North && (sc (connectionCells !! 2))) = True
    | (fc [startDown]) && (dir == South && (sc (connectionCells !! 3))) = True
    | (fc [startLeft]) && (dir == West && (sc (connectionCells !! 1))) = True
    | (fc [startRight]) && (dir == East && (sc (connectionCells !! 0))) = True
    | otherwise = False
        where {
            fc = checkIfGivenTypeCell firstCell;
            sc = checkIfGivenTypeCell secondCell;
        }

isValidMoveConnection :: Position -> Position -> Directions-> Game_Matrix -> Bool
isValidMoveConnection givenPosition nextPosition dir game
    | (x1 + x2 < xout1) || (x1 + x2 > xout2) || (y1 + y2 < yout1) || (y1 + y2 > yout2) = False
    | (firstElem (cell_data (elemAt game (x1 + x2, y1 + y2)))) == emptySpace = False
    | (connection (elemAt game givenPosition) (elemAt game (x1 + x2, y1 + y2)) dir) == False = False
    | otherwise = True
        where {
            x1 = fst givenPosition;
            x2 = fst nextPosition;
            y1 = snd givenPosition;
            y2 = snd nextPosition;
            xout1 = fst $ lowerLeftCorner(game);
            xout2 = fst $ lowerRightCorner(game);
            yout1 = snd $ lowerLeftCorner(game);
            yout2 = snd $ lowerRightCorner(game);
        }
        

checkCell :: Game_Matrix -> Position -> Bool
checkCell arr pos
    | (cellToChar $ elemAt arr pos) == startDown = True
    | (cellToChar $ elemAt arr pos) == startUp = True
    | (cellToChar $ elemAt arr pos) == startRight = True
    | (cellToChar $ elemAt arr pos) == startLeft = True
    | otherwise = False

checkIfNonFixedCell :: Game_Matrix-> Position -> Bool
checkIfNonFixedCell game pos = (elem (cellToChar (elemAt game pos)) [startUp, startDown, startLeft, startRight, winUp, winLeft, winDown, winRight, emptySpace]) == False
    

filterStartingCell :: Game_Matrix -> (Int, Int)
filterStartingCell game = head $ filter (checkCell game) $ allPermutations game

wonLevelHelper :: Level -> (Int, Int) -> Bool
wonLevelHelper givenLevel givenPosition
    | (cm (1,0) South lvl == True) && ((checkIfGivenType lvl (x + 1, y) winningCells) == True) = True
    | (cm (1,0) South lvl == True) && ((checkIfGivenType lvl (x + 1, y) winningCells) == False) = f (x + 1, y)
    | (cm (-1,0) North lvl == True) && ((checkIfGivenType lvl (x - 1, y) winningCells) == True) = True
    | (cm (-1,0) North lvl == True) && ((checkIfGivenType lvl (x - 1, y) winningCells) == False) = f (x - 1, y)
    | (cm (0,1) East lvl == True) && ((checkIfGivenType lvl (x, y + 1) winningCells) == True) = True
    | (cm (0,1) East lvl == True) && ((checkIfGivenType lvl (x, y + 1) winningCells) == False) = f (x, y + 1)
    | (cm (0,-1) West lvl == True) && ((checkIfGivenType lvl (x, y - 1) winningCells) == True) = True
    | (cm (0,-1) West lvl == True) && ((checkIfGivenType lvl (x, y - 1) winningCells) == False) = f (x, y - 1)
    | otherwise = False
        where {
            cm = isValidMoveConnection givenPosition;
            lvl = (level_state(givenLevel));
            x = fst $ givenPosition;
            y = snd $ givenPosition;
            f = wonLevelHelper $ addCell (emptySpace, givenPosition) givenLevel;
        }

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
wonLevel :: Level -> Bool
wonLevel givenLevel = wonLevelHelper givenLevel $ filterStartingCell $ (level_state givenLevel)

filterNonFixedCells :: Game_Matrix -> [Position] 
filterNonFixedCells game = filter (checkIfNonFixedCell game) $ allPermutations game

canMove :: Position -> Directions -> Game_Matrix -> Bool
canMove pos dir game
    | dir == South && valid pos (1, 0) = True
    | dir == North && valid pos (-1, 0) = True
    | dir == East && valid pos (0, 1) = True
    | dir == West && valid pos (0, -1) = True
    | otherwise = False
    where valid = isValidMove game;

filterDirections :: Game_Matrix -> Position -> [Directions]
filterDirections game pos = (filter (\dir -> (canMove pos dir game)) [East, West, North, South])

getSuccesors :: Level -> [[((Position, Directions), Level)]]
getSuccesors givenLevel = (map (\y -> (posDirList givenLevel y)) $ filterNonFixedCells (level_state(givenLevel)))
    where posDirList level pos =  map (\element -> ((pos, element), (moveCell pos element givenLevel))) $ filterDirections (level_state(level)) pos 

instance ProblemState Level (Position, Directions) where
    successors givenLevel = foldr (\element-> (element++)) [] $ getSuccesors givenLevel
    isGoal = undefined
    reverseAction = undefined
