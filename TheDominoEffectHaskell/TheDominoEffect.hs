-- The Domino Effect

import System.IO
import Data.Char
import Data.List
import Data.Maybe

-- A domino stone is a tuple of a tuple with two pips and the bone of the domino
type Dom = ((Int, Int), Int)
-- A position is a tuple with the pip and the bone it is occupied with, if the pip is not occupied, the pip is -1
type Pos = (Int, Int)
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
                        ((1, 1), 8), ((1, 2), 9),  ((1, 3), 10), ((1, 4), 11), ((1, 5), 12), ((1, 6), 13),
                                     ((2, 2), 14), ((2, 3), 15), ((2, 4), 16), ((2, 5), 17), ((2, 6), 18),
                                                   ((3, 3), 19), ((3, 4), 20), ((3, 5), 21), ((3, 6), 22),
                                                                 ((4, 4), 23), ((4, 5), 24), ((4, 6), 25),
                                                                               ((5, 5), 26), ((5, 6), 27),
                                                                                             ((6, 6), 28)]

-- Indices on the domino board
indices :: [(Int, Int)]
indices = [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7),
           (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7),
           (2, 0), (2, 1), (2, 2), (2, 3), (2, 4), (2, 5), (2, 6), (2, 7),
           (3, 0), (3, 1), (3, 2), (3, 3), (3, 4), (3, 5), (3, 6), (3, 7),
           (4, 0), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5), (4, 6), (4, 7),
           (5, 0), (5, 1), (5, 2), (5, 3), (5, 4), (5, 5), (5, 6), (5, 7),
           (6, 0), (6, 1), (6, 2), (6, 3), (6, 4), (6, 5), (6, 6), (6, 7)]

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
createRow (pos:row) = ((digitToInt pos), -1) : createRow row

-- Deletes the placed domino from the list of available dominos to place on the board
deletePlacedDominoFromDominos :: [Dom] -> Dom -> [Dom]
deletePlacedDominoFromDominos doms dom = [x | x <- doms, x /= dom]

-- Finds the indices of the row in the board
findRowInBoard :: Row -> Board -> [Int]
findRowInBoard xs xss = findIndices (== xs) xss

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
                            | fst ind == 6 && snd ind < 7  = [(6, (snd ind) - 1), (6, (snd ind) + 1), (5, (snd ind))]
                            | fst ind < 6  && snd ind == 0 = [((fst ind) - 1, 0), ((fst ind) + 1, 0), ((fst ind), 1)]
                            | fst ind < 6  && snd ind == 7 = [((fst ind) - 1, 7), ((fst ind) + 1, 7), ((fst ind), 6)]
                            | otherwise                    = [((fst ind) - 1, (snd ind)), ((fst ind) + 1, (snd ind)), ((fst ind), (snd ind) - 1), ((fst ind), (snd ind + 1))]

-- Finds the indices of the first pip of the domino in the board
findFirstPipIndices :: Pos -> Board -> [(Int, Int)]
findFirstPipIndices pos board = nub [(indb, indr)| row <- board, n <- row, n == pos, indr <- findIndicesInRow n row, indb <- findRowInBoard row board]

-- Finds the indices of the pips that have the second pip and are neighbours of the first pip
findMatchingSecondPipIndices :: Pos -> (Int, Int) -> Board -> [(Int, Int)]
findMatchingSecondPipIndices p ind board = [i | i <- findNeighbouringIndices ind, findPipOnIndex i board == p]

-- Finds the indices where the domino can be placed on the board
findMatchingIndicesWithDuplicates :: Dom -> Board -> [((Int, Int), (Int, Int))]
findMatchingIndicesWithDuplicates dom board = [(ind, nind) | ind <- findFirstPipIndices ((fst (fst dom)), -1) board, nind <- findMatchingSecondPipIndices ((snd (fst dom)), -1) ind board]

findMatchingIndices :: Dom -> Board -> [((Int, Int), (Int, Int))]
findMatchingIndices ((x,y),b) board | x == y    = dropAlternatingIndex (findMatchingIndicesWithDuplicates ((x,y),b) board)
                                    | otherwise = findMatchingIndicesWithDuplicates ((x,y),b) board

dropAlternatingIndex :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
dropAlternatingIndex [] = []
dropAlternatingIndex (i:ii:inds) = i:dropAlternatingIndex inds

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
setBoneOnPip board dom ind = replaceNthInBoard (fst ind) (replaceNthInRow (snd ind) (getPip (findPipOnIndex ind board), snd dom) (getNthInBoard (fst ind) board)) board

-- Returns the pip of a position
getPip :: Pos -> Int
getPip (pip, _) = pip

-- Returns the bone of a position
getBone :: Pos -> Int
getBone (_, bone) = bone

-- Tries to place a domino on the board and returns the resulting board
tryDomino :: Dom -> Board -> [Board]
tryDomino dom board = if length (findMatchingIndices dom board) == 1 then
                             updateBoard board dom (findMatchingIndices dom board) 
                      else if length (findMatchingIndices dom board) == 0 then
                             []
                      else
                             [board]

-- Solve the domino board
solve :: [Dom] -> Board -> [Board]
solve doms board = if length doms /= 0 then
                         do let newBoards = tryDomino (head doms) board
                            if length newBoards == 1 then
                                 do let newBoard = head newBoards
                                    if not (isEqual board newBoard) then
                                         do let newDoms = deletePlacedDominoFromDominos doms (head doms)
                                            solve newDoms newBoard
                                    else
                                         do solve ((tail doms) ++ [head doms]) board
                            else if length newBoards == 0 then
                                []
                            else
                                [head newBoards]
                   else
                         [board]

solve' :: [Dom] -> [Board] -> [Board]
solve' doms boards = if length doms == 0 then
                         boards
                     else
                         concat (map (\board -> tryDomino (head doms) board) boards)

-- Finds the number of matching indices for all available dominos
findMatchingIndicesAllDominos :: [Dom] -> Board -> [[((Int, Int),(Int, Int))]]
findMatchingIndicesAllDominos doms board = [findMatchingIndices dom board | dom <- doms]

-- Whether or not the board is completely filled
isFilled :: Board -> Bool
isFilled board = numberOfUnfilledPositions board == 0

-- Whether or not the board is equal depending on the number of unfilled positions
isEqual :: Board -> Board -> Bool
isEqual board newBoard = numberOfUnfilledPositions board == numberOfUnfilledPositions newBoard

-- Calculates the number of unfilled positions
numberOfUnfilledPositions :: Board -> Int
numberOfUnfilledPositions board = length (filter (\pos -> (getBone pos) == (-1)) (concat board))

-- Finds the pairs of indices of forced positions
findForcedPositionsIndices :: Board -> [((Int, Int), (Int, Int))]
findForcedPositionsIndices board = [(ind, nind) | ind <- indices, nind <- findOpenNeighbouringIndices board ind, length (findOpenNeighbouringIndices board ind) == 1]

-- Finds the pips on the index of the forced position
findPipsOnForcedPositionsIndex :: Board -> ((Int, Int), (Int, Int)) -> (Pos, Pos)
findPipsOnForcedPositionsIndex board ind = (findPipOnIndex (fst ind) board, findPipOnIndex (snd ind) board)

-- Finds the domino that can be placed on the index of the forced position
findDominoOnForcedPositionIndex :: [Dom] -> (Pos, Pos) -> Dom
findDominoOnForcedPositionIndex doms poss = findDomino doms (getPip (fst poss), getPip (snd poss))

-- Updates the board for one forced position
updateBoardForcedPosition :: Board -> [Dom] -> ((Int, Int), (Int, Int)) -> [Board]
updateBoardForcedPosition board doms inds = updateBoard board (findDominoOnForcedPositionIndex doms (findPipsOnForcedPositionsIndex board inds)) [inds]

-- Updates the board for all forced positions
updateBoardForcedPositions :: Board -> [Dom] -> [((Int, Int),(Int, Int))] -> [Board]
updateBoardForcedPositions board _ [] = [board]
updateBoardForcedPositions board doms inds = updateBoardForcedPositions (head (updateBoardForcedPosition board doms (head inds))) doms (tail inds)

-- Finds the indices of open neighbours of an index
findOpenNeighbouringIndices :: Board -> (Int, Int) -> [(Int, Int)]
findOpenNeighbouringIndices board ind = findOpenIndices board (findNeighbouringIndices ind) 

-- Finds the open indices from a list of indices
findOpenIndices :: Board -> [(Int, Int)] -> [(Int,Int)]
findOpenIndices board inds = filter (\ind -> getBone (findPipOnIndex ind board) == (-1)) inds

-- Finds a domino from the list of dominos based on the pips of the domino
findDomino :: [Dom] -> (Int, Int) -> Dom
findDomino doms (x,y) = head (filter (\dom -> fst dom == (x,y) || fst dom == (y,x)) doms) 


-- -- ONLY ONE BOARD REMAINS AFTER THE SOLVE METHODS
-- solveBoard :: [Dom] -> Board -> [Board]
-- solveBoard [] board = [board]
-- solveBoard doms board = do let matchingIndices = findMatchingIndicesAllDominos doms board
--                            if length (filter (\x -> length x == 0) matchingIndices) /= 0 then 
--                                  []
--                            else if length (filter (\x -> length x == 1) matchingIndices) /= 0 then
--                                  -- change board for each indices
--                                  -- find forced positions
--                                  -- recursive solveBoard if board is different from begin board
--                            else 
--                                  -- make two boards and call recursive solveBoard twice for each board

-- THIS GOES OKAY FOR ONE LOOP OF ONE FIT DOMINOS, CANNOT SOLVE THE BOARD WITH ONLY THIS
solveBoard :: [Dom] -> Board -> [Board]
solveBoard [] board = [board]
solveBoard doms board | not (null (filter (\x -> length x == 0) matchingIndices)) = []
                      | not (null (oneFitMatchingIndices))                        = solveBoard remainingDominos (placeDominos (concat oneFitMatchingIndices) fittingDominos board)
                      -- | otherwise                                                 = placeDominosMoreFit moreFitMatchingIndex fittingDominosMoreFit board
                      | otherwise                                                 = concat (map (\b -> solveBoard remainingDominosMoreFit b) (placeDominosMoreFit moreFitMatchingIndex fittingDominosMoreFit board))
                      where matchingIndices = findMatchingIndicesAllDominos doms board
                            oneFitMatchingIndices = filter (\x -> length x == 1) matchingIndices
                            moreFitMatchingIndex = head (filter (\x -> length x > 1) matchingIndices)
                            fittingDominos = findDominosOneFit board doms (concat oneFitMatchingIndices)
                            fittingDominosMoreFit = findDominosOneFit board doms moreFitMatchingIndex
                            remainingDominos = deletePlacedDominosFromDominos doms fittingDominos
                            remainingDominosMoreFit = deletePlacedDominoFromDominos doms (head fittingDominosMoreFit)

-- Place the one fit dominos on the board
placeDominos :: [((Int, Int), (Int, Int))] -> [Dom] -> Board -> Board
placeDominos [][] board = board
placeDominos (i:is) (d:ds) board = placeDominos is ds (updateBoard' board d i)

-- Updates the board by placing a domino on the list of indices
updateBoard' :: Board -> Dom -> ((Int, Int), (Int, Int)) -> Board
updateBoard' board dom i = setBoneOnPip (setBoneOnPip board dom (fst i)) dom (snd i)

-- Finds the dominos that fit on the one fit indices
findDominosOneFit :: Board -> [Dom] -> [((Int, Int), (Int, Int))] -> [Dom]
findDominosOneFit board doms is = [findDominoOnOneFitIndex board doms i | i <- is]

-- Finds the pips on the index of the forced position
findDominoOnOneFitIndex :: Board -> [Dom] -> ((Int, Int), (Int, Int)) -> Dom
findDominoOnOneFitIndex board doms ind = findDomino doms (getPip (findPipOnIndex (fst ind) board), getPip(findPipOnIndex (snd ind) board))

-- Deletes the placed domino from the list of available dominos to place on the board
deletePlacedDominosFromDominos :: [Dom] -> [Dom] -> [Dom]
deletePlacedDominosFromDominos ds dds = [d | d <- ds, not (elem d dds)]

-- Place the one fit dominos on the board
placeDominosMoreFit :: [((Int, Int), (Int, Int))] -> [Dom] -> Board -> [Board]
placeDominosMoreFit (i:[]) (d:[]) board = [updateBoard' board d i]
placeDominosMoreFit (i:is) (d:ds) board = (updateBoard' board d i):(placeDominosMoreFit is ds board)
