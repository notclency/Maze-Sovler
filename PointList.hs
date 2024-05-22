module PointList(

    PointList,
    SolutionList,

    addToList,
    removeFromList,
    addToSol,
    updateMaze,
    validateAll,
    getPoint,
    findAndReplace
)
where 

import Maze

----------------------- Data types -----------------------

{--
    Data Type: List of Points
    Description: A list of points to represent the solution path.
--}
type PointList = [Point]

{--
    Data Type: Solution List
    Description: A list of lists of points to represent a collection of solution paths.
--}
type SolutionList = [[Point]]

----------------------- Functions Definitions -----------------------
{--
    Function: addToList
    Description: Adds a point to a list of points.
    Parameters: Point - the point to be added, PointList - the list of points
    Returns: PointList - the updated list of points
--}
addToList :: Point -> PointList -> PointList

{--
    Function: removeFromList
    Description: Removes a point from a list of points.
    Parameters: Point - the point to be removed, PointList - the list of points
    Returns: PointList - the updated list of points
--}
removeFromList :: Point -> PointList -> PointList

{--
    Function: addToSol
    Description: Adds a list of points to a solution list.
    Parameters: PointList - the list of points to be added, SolutionList - the solution list
    Returns: SolutionList - the updated solution list
--}
addToSol :: PointList -> SolutionList -> SolutionList

{--
    Function: validateAll
    Description: Validates all points in a solution list.
    Parameters: SolutionList - the solution list
    Returns: SolutionList - the updated solution list
--}
validateAll :: SolutionList -> SolutionList

{--
    Function: getPoint
    Description: Gets a point from the maze.
    Parameters: Int - the x-coordinate, Int - the y-coordinate, Maze - the maze
    Returns: Point - the point
--}
findAndReplace :: Point -> Maze -> Maze

{--
    Function: updateMaze
    Description: Updates the maze with the solution path.
    Parameters: SolutionList - the solution list, Maze - the maze
    Returns: Maze - the updated maze
--}
updateMaze :: SolutionList -> Maze -> Maze

{--
    Function: updateMazeRow
    Description: Updates a row in the maze with the solution path.
    Parameters: PointList - the list of points, Maze - the maze
    Returns: Maze - the updated maze
--}
updateMazeRow :: PointList -> Maze -> Maze


----------------------- Functions -----------------------

addToList p ps = ps ++ [p]

removeFromList _ [] = []
removeFromList p (ps:pss) 
    | p == ps = pss
    | otherwise = ps : removeFromList p pss

validateAll [] = []
validateAll (x:xs) = validateList x : validateAll xs

updateMaze [] maze = maze
updateMaze (x:xs) maze = updateMaze xs (updateMazeRow x maze)

updateMazeRow [] maze = maze
updateMazeRow (x:xs) maze = updateMazeRow xs (findAndReplace x maze)

addToSol [] sol = sol
addToSol p sol = sol ++ [p]

findAndReplace newPoint maze = map (map replace) maze
  where
    replace point
      | point == newPoint = newPoint
      | otherwise = point

----------------------- Helper Functions -----------------------
{--
    Function: instance of Eq for Point
    Description: Compares two points.
    Parameters: Point - the first point, Point - the second point
    Returns: Bool - whether the points are equal or not
--}
instance Eq Point where
    p1 == p2 = (x p1 == x p2) && (y p1 == y p2)

{--
    Function: getPoint
    Description: Gets a point from the maze.
    Parameters: Int - the x-coordinate, Int - the y-coordinate, Maze - the maze
    Returns: Point - the point
--}
getPoint :: Int -> Int -> Maze -> Point
getPoint x y maze
  | x < 0 || y < 0 = Point (-1) (-1) False False False
  | y >= length maze || x >= length (head maze) = Point (-1) (-1) False False False
  | otherwise = (maze !! y) !! x