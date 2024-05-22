import SolveMaze
import Maze
import PointList

main :: IO ()
main = do
    maybeMazeData <- extractData
    case maybeMazeData of
        Just mazeData -> do
            let filledMaze = loadMaze mazeData
                entryPoint = getEntryPoint mazeData
                exitPoint = getExitPoint mazeData

            let loadedMaze = findAndReplace entryPoint filledMaze
                finalMaze = findAndReplace exitPoint loadedMaze

            let list = solveMaze entryPoint exitPoint [] [[]] finalMaze
            putStr "\nSolution List(s):" >> printPoints list

            let updatedMaze = updateMaze list finalMaze
            putStr "Updated Maze: \n"
            printMaze updatedMaze

        Nothing -> do 
            putStr "<main>: "
            putStr red
            putStr "error: "
            putStr def
            putStr "program aborted.\n"
            putStr def
            where
                def = "\ESC[0m"
                red = "\ESC[91m"
                green = "\ESC[32m"

