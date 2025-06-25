import System.Random
import Data.List

type Color     = String
type Colors    = [Color]

colors :: Colors
colors = ["blue", "red", "yellow", "green", "white"]
gameSize = 4


--  Row represents a row on the board
type Row       = [Color]

--  Answer is a pair where the left component represents the number of black pegs and
--  the right component represents the number of white pegs
--
-- We find a black peg for each correct guess (correct color and position)
-- We find a white peg for each peg that belongs in the solution but is incorrectly positioned.

type Answer    = (Int, Int)

-- A board is represented as a list of rows and answers
type Board     = [(Row, Answer)]

printBoard :: Board -> IO ()
printBoard board = let print [] = putStr ("\n")
                       print (x:xs) = do putStr (show x)
                                         putStr ("\n")
                                         print xs
                   in do putStr ("\n BOARD\n")
                         print board


validGuess :: Int -> Colors -> Row -> Bool
validGuess size colors guess
        = length guess == size && and [elem x colors | x <- guess]



answerIs :: Row -> Row -> Answer
answerIs answer guess
        = (black, white)
          where
                black = length [ g | (g,a) <- zip guess answer, g == a]
                white = length (mintersect guess answer) - black

mintersect :: Eq a => [a] -> [a] -> [a]
mintersect [] ys  = []
mintersect (x:xs) ys
        | elem x ys   = x:mintersect xs (mdelete x ys)
        | otherwise   = mintersect xs ys

mdelete :: Eq a => a -> [a] -> [a]
mdelete x [] = []
mdelete x (y:ys)
        | x == y    = ys
        | otherwise = y:mdelete x ys

-- mastermind using some "AI" player
mmind  ::
     Int                                       -- Length of each row
     -> Colors                                 -- Set of colors
     -> (Int -> Colors -> Colors -> Bool)      -- Checks if guess is valid
     -> (Colors -> Colors -> Answer)           -- Compares guess against hidden row
     -> (Int -> Colors -> Board -> IO Row)        -- Some "AI" player
     -> IO [(Colors, Answer)]
mmind size colors validGuess answerIs player =
       let answerIsIO guess hiddenrow = return (answerIs guess hiddenrow)
           buildboardIO board guess answerIs = return (board ++ [(guess,answerIs)])
           initialboard = []
           playitIO hiddenrow board =
                           do newguess <- player size colors board
                              if validGuess size colors newguess then return ()
                               else putStr ("\n Your player doesn't produce correct guesses\n")
                              newanswerIs <- answerIsIO newguess hiddenrow
                              newboard <- buildboardIO board newguess newanswerIs
                              printBoard newboard
                              if newanswerIs == (size,0) then (return newboard)
                               else (playitIO hiddenrow newboard)
        in do hiddenrow <- randomRowIO size colors
              playitIO hiddenrow initialboard


randomRowIO :: Int -> Colors -> IO Colors
randomRowIO size colors = do
     xs <- mapM (\_ -> randomRIO (0::Int, size-1)) [1..size]
     return [colors!!x | x <- xs]

naive_player :: Int -> Colors -> Board -> IO Row
naive_player size colors _ = do
  row <- randomRowIO size colors
  return row



-------------------------------------------------------------------------------------
------------------------ Implementation of an smart Player --------------------------
------------------- following Donald Knuth MinMax Algorithm -------------------------
-- https://www.cs.uni.edu/~wallingf/teaching/cs3530/resources/knuth-mastermind.pdf --
-------------------------------------------------------------------------------------

-- helpfull predefined functions / values 
allPossibleCodesPreCalc = allPossibleCodes gameSize colors
compFunc (_,s1) (_, s2) = compare s1 s2

-- Returns all possible codes
allPossibleCodes :: Int -> Colors -> [Row]
allPossibleCodes n colors = sequence (replicate n colors)

-- filter an code list based of an answer and guess 
filterCodes :: Row -> Answer -> [Row] -> [Row]
filterCodes guess answer possibleCodes =
  filter (\code -> answerIs code guess == answer) possibleCodes

-- choose next best move
-- "best" meaning the next move where the number of remaining possible codes is minimal
chooseBest :: Int      
              -> Colors 
              -> [Row]
              -> Row
chooseBest size colors possibleCodes =
  let
    -- scores a guess by how many codes are possible after that guess
    scoreGuess :: Row -> Int
    scoreGuess currentGuess =
      let
        -- for each possible answer determine how many codes would produce this answer
        -- 1. generate all (b,w) combinations
        -- 2. filter only valid combinations -> (3,3) is not allowed if size is 4
        -- 3. filter possible codes by checking with answerIs 
        -- 4. count number of results
        answerCounts = [ length (filter (\code -> answerIs currentGuess code == ans) possibleCodes)
                         | b <- [0..size]     -- generator 1
                         , w <- [0..size]     -- generator 1
                         , b + w <= size      -- filter 1
                         , b >= 0, w >= 0     -- filter 2
                         , let ans = (b,w)    -- local definition of possible answer
                       ]
      in
        -- select worst case
        maximum (0:answerCounts)

    --allGuesses = allPossibleCodes size colors
    allGuesses = allPossibleCodesPreCalc

    -- find the smallest maximum (MinMax)
    -- get pairs of (guess, score) -> get minimum -> get first element of tupel (the guess)
    bestGuess = fst (minimumBy compFunc $ [ (g, scoreGuess g) | g <- allGuesses ])
  in
    bestGuess


player :: Int         
          -> Colors   
          -> Board
          -> IO Row
player size colors board = do
  let
    -- generate all codes
    allInitialCodes = allPossibleCodes size colors
    -- filter all codes based on the current game state
    -- foldl takes allInitialCodes (as initial akkumulator) and each game round in board as arguments of the provided funktion
    -- based on each game round the initial set of codes is reduced 
    nextCodes = foldl (\codes (guess, ans) -> filterCodes guess ans codes)
                                  allInitialCodes
                                  board

  -- choose last move or next best 
  -- "original" Algorithm would start with an guess [X,X,Y,Y]
  -- but we don't need that here for the sake of flexibility
  case nextCodes of
    [code] -> return code
    _      -> return $ chooseBest size colors nextCodes 


----------------------------------------------------
--        End of own Implementation               --
----------------------------------------------------

play_mm = mmind gameSize colors validGuess answerIs player

main = play_mm