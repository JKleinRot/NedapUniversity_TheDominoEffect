-- The Domino Effect

import System.IO
import Data.Char

-- A domino stone is a tuple with two pips
type Dom = (Int, Int)
-- A position is a tuple with the pip, whether or not the pip is occupied and the bone it is occupied with, if the pip is not occupied, the pip is -1
type Pos = (Int, Bool, Int)
-- A board is a list with a list of positions
type Board = [[Pos]]

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
dominos = [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6),
                   (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6),
                           (2, 2), (2, 3), (2, 4), (2, 5), (2, 6),
                                   (3, 3), (3, 4), (3, 5), (3, 6),
                                           (4, 4), (4, 5), (4, 6),
                                                   (5, 5), (5, 6),
                                                           (6, 6)]

-- Creates a board from the input grid
createBoard :: String -> Board
createBoard input = map createInitialPositions (splitToRows input)

-- Splits the input grid into rows
splitToRows :: String -> [String]
splitToRows [] = []
splitToRows input = [take 8 input] ++ splitToRows (drop 8 input)

-- Creates the initial positions of the board from the rows of the input grid
createInitialPositions :: String -> [Pos]
createInitialPositions [] = []
createInitialPositions (pos:row) = ((digitToInt pos), False, -1) : createInitialPositions row
