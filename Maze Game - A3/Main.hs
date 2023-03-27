module Main (get_maze, print_maze, is_wall, place_player, move, can_move, game_loop, get_path, main) where 

import System.Environment

maze_path = "J:\\Liv Uni\\COMP105 Programming language paradigms\\Coursework 3\\maze2.txt"

-- Useful code from Lecture 25
-- You may use this freely in your solutions

get :: [String] -> Int -> Int -> Char
get maze x y = (maze !! y) !! x 

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after

set :: [String] -> Int -> Int -> Char -> [String]
set maze x y char = 
    let
        line = maze !! y
        new_line = modify_list line x char
        new_maze = modify_list maze y new_line
    in
        new_maze

---- Part A

-- Question 1

get_maze :: String -> IO [String]
get_maze maze_path =
 do
  lineList <- readFile maze_path
  let x = lines(lineList)
  return x

-- Question 2

print_maze :: [String] -> IO ()
print_maze x = 
 do
  let y = unlines(x)
  z <- putStrLn(y)
  return z

-- Question 3

is_wall :: [String] -> (Int, Int) -> Bool
is_wall m cord = 
 if get m (fst(cord)) (snd(cord)) == '#' then True else False
 

-- Question 4

place_player :: [String] -> (Int, Int) -> [String]
place_player m cord = set m (fst(cord)) (snd(cord)) '@'


---- Part B

-- Question 5

move :: (Int, Int) -> Char -> (Int, Int)
move cord key = 
 if key == 'w' && (snd(cord)) /= 0 then
  (fst(cord), snd(cord) - 1)
 else if key == 's' then
  (fst(cord), snd(cord) + 1)
 else if key == 'a' && (fst(cord)) /= 0 then
  (fst(cord) - 1, snd(cord))
 else if key == 'd' then
  (fst(cord) + 1, snd(cord))
 else 
  cord

-- Question 6

can_move :: [String] -> (Int, Int) -> Char -> Bool
can_move m cord key = 
 if key == 'w' then
   if is_wall m ((fst(cord), snd(cord) - 1)) == False then True else False
 else if key == 's' then
  if is_wall m ((fst(cord), snd(cord) + 1)) == False then True else False
 else if key == 'a' then
  if is_wall m ((fst(cord) - 1, snd(cord))) == False then True else False
 else if key == 'd' then
  if is_wall m ((fst(cord) + 1, snd(cord))) == False then True else False
 else 
  False

-- Question 7

game_loop :: [String] -> (Int, Int) -> IO ()
game_loop m cords = 
 do
  print_maze (place_player m cords)
  x <- getLine
  let key = head(x) 
  if can_move m cords key == True then 
   game_loop m (move cords key)
  else
   game_loop m cords




---- Part C

-- Question 8

get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path = error "Not implemented"

-- Question 9

main :: IO ()
main = error "Not implemented"
