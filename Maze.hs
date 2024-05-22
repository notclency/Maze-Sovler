module Maze
(
    Point(..),
    Maze,

    printMaze,                  -- prints the maze (2D Array)
    printPoints,            -- prints the maze (2D Array of points and their data) for debugging purposes
    validateList,               -- validates the list of points
    loadMaze,                   -- loads the data extracted from file into a maze (2D Array of points)
    extractData,                -- extracts the data from file
    getEntryPoint,              -- gets the entry point of the maze
    getExitPoint,                 -- gets the exit point of the maze
)
where

import Prelude
import Data.Char (isDigit)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch)

------------------ Functions Definitions------------------
{--
    Function: printMaze
    Description: Prints the maze (2D Array) to the console.
    Parameters: Maze
    Returns: IO ()
--}
printMaze :: Maze -> IO ()

{--
    Function: printMazePoints
    Description: Prints the maze (2D Array of points and their data) to the console.
    Parameters: Maze
    Returns: IO ()
--}
printPoints :: Maze -> IO ()

{--
    Function: loadMaze
    Description: Loads the data extracted from file into a maze (2D Array of points).
    Parameters: [[Int]] - the data extracted from file
    Returns: IO Maze
--}
loadMaze :: [[Int]] -> Maze

{--
    Function: extractData
    Description: Extracts entry point from extracted data.
    Parameters: [[Int]] - the data extracted from file
    Returns: IO Point
--}
getEntryPoint :: [[Int]] -> Point

{--
    Function: extractData
    Description: Extracts exit point from extracted data.
    Parameters: [[Int]] - the data extracted from file
    Returns: IO Point
--}
getExitPoint :: [[Int]] -> Point

{--
    Function: validateList
    Description: Validates the list of points as a solution path.
    Parameters: [Point] - the list of points
    Returns: [Point]
--}
validateList :: [Point] -> [Point]

------------------ Data types ------------------

{--
    Data type: Point
    Description: Represents a point in the maze.
    Fields:
        x - the x-coordinate of the point
        y - the y-coordinate of the point
        isPath - whether the point is a path or not
        visited - whether the point has been visited or not
        solution - whether the point is part of the solution path or not
--}
data Point = Point { x :: Int,
                     y :: Int,
                     isPath :: Bool,
                     visited :: Bool,
                     solution :: Bool
                     }

{--
    Data type: Maze
    Description: Represents a maze as a 2D Array of points.
--}
type Maze = [[Point]]

------------------ Functions ------------------

printMaze [] = putStrLn ""
printMaze (ps:pss) = printMazeRow ps >> printMaze pss

-- For debugging purposes
printPoints [] = putStrLn ""
printPoints (ps:pss) = putStr "\n -- " >> printPointsRow ps >> printPoints pss


validateList [] = []
validateList (p:ps) = p {solution = True} : validateList ps

-- Load Maze 
loadMaze mazeData = fillMaze 0 0 (drop 2 mazeData)

getEntryPoint (xs:xss) = Point (head xs) (xs !! 1) True False False
getExitPoint (xs:xss:xsss) = Point (head xss) (xss !! 1) True False False

------------------ Helper functions (private functions) ------------------
{--
    Function: instance of Show for Point
    Description: Converts a point to a string.
    Parameters: Point - the point to convert
    Returns: String
--}
instance Show Point where
    show p = "(" ++ show (x p) ++ ", " ++ show (y p) ++  ") "

{--
    Function: boolToString
    Description: Converts a boolean to a string.
    Parameters: Bool - the boolean to convert
    Returns: String
--}
boolToString :: Bool -> String
boolToString b = if b then "T" else "F"

{--
    Function: intToBool
    Description: Converts an integer to a boolean.
    Parameters: Int - the integer to convert
    Returns: Bool
--}
intToBool :: Int -> Bool
intToBool n | n == 1 = True
            | otherwise = False

{--
    Function: boolToInt
    Description: Converts a boolean to an integer.
    Parameters: Bool - the boolean to convert
    Returns: Int
--}
boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

{--
    Function: printPointsRow
    Description: Prints a row of points to the console.
    Parameters: [Point] - the row of points
    Returns: IO ()
--}
printPointsRow :: [Point] -> IO ()
printPointsRow [] = putStrLn ""
printPointsRow (p:ps) = putStr (show p) >> printPointsRow ps

{--
    Function: extractData
    Description: Extracts the data from file.
    Parameters: None
    Returns: IO (Maybe [[Int]])
--}
extractData :: IO (Maybe [[Int]])
extractData = do
    putStr "\nEnter the name of the file: "
    filepath <- getLine
    content <- catch (readFile filepath) handler
    let linesOfFile = lines content
    let tokens = map words linesOfFile
    let mazeData = drop 2 (map (map read . filter (all isDigit)) tokens)
    return (if null content then Nothing else Just mazeData)
  where
    handler e
      | isDoesNotExistError e = do
          putStr "<extractData>: "
          putStr red
          putStr "error: "
          putStr def
          putStrLn "file does not exist."
          return ""
        | otherwise = ioError e
            where
                def = "\ESC[0m"
                red = "\ESC[91m"
                green = "\ESC[32m"

{--
    Function: printMazeRow
    Description: Prints a row of the maze to the console.
    Parameters: [Point] - the row of the maze
    Returns: IO ()
--}
printMazeRow :: [Point] -> IO ()
printMazeRow [] = putStrLn ""
printMazeRow (p:ps)
    | isPath p && solution p = do
        putStr green
        putStr $ show $ boolToInt $ isPath p
        putStr " "
        putStr def
        printMazeRow ps
    | isPath p && not (solution p) = do
        putStr $ show $ boolToInt $ isPath p
        putStr " "
        printMazeRow ps
    | otherwise = do
        putStr red
        putStr $ show $ boolToInt $ isPath p
        putStr " "
        putStr def
        printMazeRow ps
    where
        def = "\ESC[0m"
        red = "\ESC[91m"
        green = "\ESC[32m"

{--
    Function: fillMaze
    Description: Fills the maze with points.
    Parameters: Int - the x-coordinate of the point
                Int - the y-coordinate of the point
                [[Int]] - the data extracted from file
    Returns: Maze
--}
fillMaze :: Int -> Int -> [[Int]] -> Maze
fillMaze _ _ [] = []
fillMaze x y (xs:xss) = fillRow x y xs : fillMaze x (y + 1) xss

{--
    Function: fillRow
    Description: Fills a row of the maze with points.
    Parameters: Int - the x-coordinate of the point
                Int - the y-coordinate of the point
                [Int] - the row of the data extracted from file
    Returns: [Point]
--}
fillRow :: Int -> Int -> [Int] -> [Point]
fillRow _ _ [] = []
fillRow x y (xs:xss) = Point x y (intToBool xs) False False : fillRow (x + 1) y xss
