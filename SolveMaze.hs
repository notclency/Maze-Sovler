module SolveMaze
(
    solveMaze,
)
where

import Maze
import PointList

{--
    Function: solveMaze
    Description: Solves the maze by recursively finding the path from the entry point to the exit point.
    Parameters: Point - the entry point, Point - the exit point, PointList - the list of points, SolutionList - the list of solution paths, Maze - the maze
    Returns: SolutionList - the list of solution paths
--}
solveMaze :: Point -> Point -> PointList -> SolutionList -> Maze -> SolutionList
solveMaze entryPoint exitPoint solutionList finList maze
    | not (isPath entryPoint) || visited entryPoint = finList
    | entryPoint == exitPoint = validateAll (addToSol currList finList)
    | otherwise = foldl solveDirection right_list directions
    where
        currPoint = entryPoint{visited = True}
        currList = addToList currPoint solutionList
        newMaze = findAndReplace currPoint maze
        directions = [(0, 1), (-1, 0), (0, -1), (1, 0)]  -- down, left, up, right
        right_list = finList
        solveDirection list (dx, dy) = solveMaze newPoint exitPoint currList list newMaze
            where newPoint = getPoint (x currPoint + dx) (y currPoint + dy) newMaze
