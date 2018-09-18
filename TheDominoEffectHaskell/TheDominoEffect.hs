-- The Domino Effect

import System.IO
import Data.Char
import Data.List
import Data.Maybe

-- A domino stone is a tuple of a tuple with two pips and the bone of the domino
type Dom = ((Int, Int), Int)
-- A position is a tuple with the pip, whether or not the pip is occupied and the bone it is occupied with, if the pip is not occupied, the pip is -1
type Pos = (Int, Bool, Int)
-- A row is a list of position
type Row = [Pos]
-- A board is a list with rows
type Board = [Row]

-- The main function
theDominoEffect :: IO ()
theDominoEffect = do putStrLn "Please enter the 7 x 8 domino grid. Enter each number starting at the top left number and work your way down row wise to the bottom right number of the grid: "
                     input <- getLine
                     if (isValidInput input) then 
                             do let board = createBoard(input)
                                putStrLn (show board)
                     else 
                             do putStrLn "The entered domino grid was not valid."
                                theDominoEffect

-- Checks whether the input grid is valid
isValidInput :: String -> Bool
isValidInput input = if not (areCorrectDigits input) then 
                             False 
                     else if length input /= 56 then 
                             False 
                     else if not (isValidGrid input) then
                             False
                     else 
                             True

-- Checks whether the input grid only consists of the digits 0..6
areCorrectDigits :: String -> Bool
areCorrectDigits [] = True
areCorrectDigits (x:xs) = isDigit x && digitToInt x <= 6 && areCorrectDigits xs

-- Checks whether the input grid contains each valid digit eight times
isValidGrid :: String -> Bool
isValidGrid input = length (filter (== '0') input) == 8 && length (filter (== '1') input) == 8 && length (filter (== '2') input) == 8 && length (filter (== '3') input) == 8 &&
                    length (filter (== '4') input) == 8 && length (filter (== '5') input) == 8 && length (filter (== '6') input) == 8

-- Available domino stones
dominos :: [Dom]
dominos = [((0, 0), 1), ((0, 1), 2), ((0, 2), 3),  ((0, 3), 4),  ((0, 4), 5),  ((0, 5), 6),  ((0, 6), 7),
                        ((1, 1), 8), ((2, 2), 9),  ((1, 3), 10), ((1, 4), 11), ((1, 5), 12), ((1, 6), 13),
                                     ((2, 2), 14), ((2, 3), 15), ((2, 4), 16), ((2, 5), 17), ((2, 6), 18),
                                                   ((3, 3), 19), ((3, 4), 20), ((3, 5), 21), ((3, 6), 22),
                                                                 ((4, 4), 23), ((4, 5), 24), ((4, 6), 25),
                                                                               ((5, 5), 26), ((5, 6), 27),
                                                                                             ((6, 6), 28)]

-- Creates a board from the input grid
createBoard :: String -> Board
createBoard input = map createRow (splitToRows input)

-- Splits the input grid into rows
splitToRows :: String -> [String]
splitToRows [] = []
splitToRows input = [take 8 input] ++ splitToRows (drop 8 input)

-- Creates a row on the board from a row of the input grid
createRow :: String -> [Pos]
createRow [] = []
createRow (pos:row) = ((digitToInt pos), False, -1) : createRow row

-- Deletes the placed domino from the list of available dominos to place on the board
deletePlacedDominoFromDominos :: [Dom] -> Dom -> [Dom]
deletePlacedDominoFromDominos doms dom = [x | x <- doms, x /= dom]

-- Finds the index of the row in the board
findRowInBoard :: Row -> Board -> [Int]
findRowInBoard xs xss = case (findIndex (== xs) xss) of
                                 Nothing    -> []
                                 Just index -> [index]

-- Finds the indices of a pip in a row
findIndicesInRow :: Pos -> Row -> [Int]
findIndicesInRow x xs = findIndices (== x) xs

-- Gets the nth element from a row
getNthInRow :: Int -> Row -> Pos
getNthInRow 0 xs = head xs
getNthInRow n xs = getNthInRow (n - 1) (tail xs)

-- Gets the nth row from a board
getNthInBoard :: Int -> Board -> Row
getNthInBoard 0 xss = head xss
getNthInBoard n xss = getNthInBoard (n - 1) (tail xss)

-- Finds the pip on the board at the specified index
findPipOnIndex :: (Int, Int) -> Board -> Pos
findPipOnIndex ind board = getNthInRow (snd ind) (getNthInBoard (fst ind) board)

-- Finds the neighbouring pip indices for a given pip index
findNeighbouringIndices :: (Int, Int) -> [(Int, Int)]
findNeighbouringIndices ind | fst ind == 0 && snd ind == 0 = [(0, 1), (1, 0)]
                            | fst ind == 0 && snd ind == 7 = [(0, 6), (1, 7)]
                            | fst ind == 6 && snd ind == 0 = [(6, 1), (5, 0)]
                            | fst ind == 6 && snd ind == 7 = [(6, 6), (5, 7)]
                            | fst ind == 0 && snd ind < 7  = [(0, (snd ind) - 1), (0, (snd ind) + 1), (1, (snd ind))]
                            | fst ind == 6 && snd ind > 0  = [(6, (snd ind) - 1), (6, (snd ind) + 1), (5, (snd ind))]
                            | fst ind < 7  && snd ind == 0 = [((fst ind) - 1, 0), ((fst ind) + 1, 0), ((fst ind), 1)]
                            | otherwise                    = [((fst ind) - 1, (snd ind)), ((fst ind) + 1, (snd ind)), ((fst ind), (snd ind) - 1), ((fst ind), (snd ind + 1))]

-- Finds the indices of the first pip of the domino in the board
findFirstPipIndices :: Pos -> Board -> [(Int, Int)]
findFirstPipIndices pos board = nub [(head (findRowInBoard row board), ind)| row <- board, n <- row, n == pos, ind <- findIndicesInRow n row]

-- Finds the indices of the pips that have the second pip and are neighbours of the first pip
findMatchingSecondPipIndices :: Pos -> (Int, Int) -> Board -> [(Int, Int)]
findMatchingSecondPipIndices p ind board = [i | i <- findNeighbouringIndices ind, findPipOnIndex i board == p]

-- Finds the indices where the domino can be placed on the board
findMatchingIndices :: Dom -> Board -> [((Int, Int), (Int, Int))]
findMatchingIndices dom board = [(ind, nind) | ind <- findFirstPipIndices ((fst (fst dom)), False, -1) board, nind <- findMatchingSecondPipIndices ((snd (fst dom)), False, -1) ind board]

-- Updates the board by placing a domino on the list of indices
updateBoard :: Board -> Dom -> [((Int, Int), (Int, Int))] -> [Board]
updateBoard board dom inds = [setBoneOnPip (setBoneOnPip board dom (fst ind)) dom (snd ind) | ind <- inds]

-- Replaces the position at the index with the specified position
replaceNthInRow :: Int -> Pos -> Row -> Row
replaceNthInRow n pos (x:xs) | n == 0    = pos:xs
                             | otherwise = x:replaceNthInRow (n - 1) pos xs

-- Replaces the row at the index with the specified row
replaceNthInBoard :: Int -> Row -> Board -> Board
replaceNthInBoard n row (x:xs) | n == 0    = row:xs
                               | otherwise = x:replaceNthInBoard (n - 1) row xs

-- Sets the bone of the position to the correct bone of the placed domino
setBoneOnPip :: Board -> Dom -> (Int, Int) -> Board
setBoneOnPip board dom ind = replaceNthInBoard (fst ind) (replaceNthInRow (snd ind) (getPip (findPipOnIndex ind board), True, snd dom) (getNthInBoard (fst ind) board)) board

-- Returns the pip of a position
getPip :: Pos -> Int
getPip (pip, _, _) = pip

-- Returnst he bone of a position
getBone :: Pos -> Int
getBone (_, _, bone) = bone

-- Tries to place a domino on the board and returns the resulting boards
tryDomino :: Dom -> Board -> [Board]
tryDomino dom board = updateBoard board dom (findMatchingIndices dom board)

-- Solve the domino board
solve :: [Dom] -> Board -> [Board]
solve doms board = tryDomino (head doms) board


