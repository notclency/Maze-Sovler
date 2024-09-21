# Maze Solver in Haskell #

## Overview ##
This project implements a maze-solving algorithm in Haskell. The system can read a maze from a file, solve it, and display the solution. The main components of the project are:

**Main.hs:** Entry point for the program, responsible for initializing the maze-solving process.\
**Maze.hs:** Defines the structure of the maze, parsing input and handling maze logic.\
**SolveMaze.hs:** Implements the solving algorithm that finds a path through the maze.

## Features ##
**Viewpoint Invariance:** The system can handle mazes represented in different formats or perspectives.\
**Partial Maze Recognition:** It can solve partially defined or incomplete mazes.\
**Customizable Input:** Supports maze input from various sources, allowing flexibility in how the maze is presented.

## Installation ##
To run the maze solver:

***Clone this repository:***
```
git clone https://github.com/yourusername/maze-solver-haskell.git
cd maze-solver-haskell
```
***Ensure you have the Haskell compiler (ghc) installed.***

***Compile the project:***
```
bash
Copy code
ghc -o solveMaze Main.hs
```
## Usage ##

Prepare a maze file in a compatible format.
Run the compiled program:
bash
```
./solveMaze maze.txt
```
The solution to the maze, if found, will be printed on the console.
