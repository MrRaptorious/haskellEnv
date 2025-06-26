import System.Random
import Data.List (nub, foldl')
import Control.Exception (catch, SomeException) -- Added for error handling

type Color     = String
type Colors    = [Color]

colors :: Colors
colors = ["blue", "red", "yellow", "green", "white"]


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

-- MASTERMIND PLAYER IMPLEMENTING DONALD KNUTH'S MINIMAX ALGORITHM
-- How the algorithm finds the solution:
-- 1. Start with 'S', the list of all possible secret codes.
-- 2. On each turn:
--    a. Filter 'S': Remove codes that don't match the feedback from previous guesses.
--       (Example: If your guess "red blue" got 1 black peg, any code in 'S' that wouldn't
--       give 1 black peg against "red blue" is removed).
--    b. Evaluate all *possible guesses*:
--       i. For each potential guess, calculate the black and white peg feedback you would get,
--          if that potential guess were played and each code still in 'S' was the actual secret.
--       ii. For each possible type of feedback (e.g., 1 black, 0 white; 0 black, 2 white),
--           count how many codes in 'S' would lead to that specific feedback if the potential guess were made.
--    c. Find the 'worst-case' for each potential guess: This is the largest count found in step 2b-ii,
--       representing the maximum number of codes that could still be in 'S' if you made this guess.
--    d. Choose the next guess: Pick the potential guess that has the smallest worst-case count.
--       This minimizes the maximum number of remaining possibilities.
-- 3. Repeat until 'S' has only one code left (which is the solution) or you guess correctly.

-- Generates all possible codes of a given size from the available colors.
-- This creates a Cartesian product of colors for 'size' positions.
allPossibleCodes :: Int -> Colors -> [Row]
allPossibleCodes 0 _ = [[]] -- Base case: for size 0, only an empty list is possible
allPossibleCodes size cs = [ c : rest | c <- cs, rest <- allPossibleCodes (size - 1) cs ]


-- Filters the set of possible secret codes (S) based on the board history.
-- For each past guess and its feedback, it removes codes from 'currentS'
-- that are inconsistent with that feedback.
filterPossibleCodes :: [Row] -> Board -> [Row]
filterPossibleCodes initialS [] = initialS -- If no board history, all initialS are possible
filterPossibleCodes currentS ((guess, answer) : restOfBoard) =
    let -- Filter codes 's' from 'currentS' that would yield 'answer' if 'guess' were the secret
        filteredS = [ s | s <- currentS, answerIs s guess == answer ]
    in filterPossibleCodes filteredS restOfBoard -- Recursively filter with the rest of the board


-- Generates all valid (black, white) peg combinations for a given code length (size).
-- For example, for size 4: (4,0), (0,0), (1,2), etc.
possibleAnswers :: Int -> [Answer]
possibleAnswers size =
    [ (b, w) | b <- [0..size]     -- Black pegs can range from 0 to size
             , w <- [0..size - b] -- White pegs can range from 0 to (size - black pegs)
    ]

-- Calculates the maximum number of remaining possible secret codes ('currentS')
-- for a given 'candidate' guess across all possible feedback answers.
-- This represents the "worst-case" group size if 'candidate' were played.
calculateMaxRemaining :: [Row] -> [Answer] -> Row -> Int
calculateMaxRemaining currentS allPossibleAnswers candidate =
    let counts = [ length [ s | s <- currentS, answerIs s candidate == ans ]
                 | ans <- allPossibleAnswers ]
    in if null counts then 0 else maximum counts

-- Determines if a 'candidateG' is a better choice than the 'currentBestG'
-- based on their minimax scores and Knuth's tie-breaking rule.
-- Returns True if 'candidateG' has a smaller worst-case score, or if scores are equal
-- and 'candidateG' is in 'currentS' while 'currentBestG' is not.
isBetterGuess :: Row -> Int -> Row -> Int -> [Row] -> Bool
isBetterGuess currentBestG currentMinMaxScore candidateG candidateMaxScore currentS =
    candidateMaxScore < currentMinMaxScore
    || (candidateMaxScore == currentMinMaxScore
        && elem candidateG currentS
        && not (elem currentBestG currentS))

-- Recursively finds the best guess from a list of 'remainingCandidates'.
-- It maintains the 'currentBestG' found so far and its 'currentMinMaxScore'.
-- It calls 'calculateMaxRemaining' and 'isBetterGuess' for each candidate.
findBestGuessRecursive :: [Row] -> [Answer] -> [Row] -> Row -> Int -> Row
findBestGuessRecursive currentS allPossibleAnswers [] bestG _ = bestG
findBestGuessRecursive currentS allPossibleAnswers (candidateG : remainingCandidates) currentBestG currentMinMaxScore =
    let candidateMaxScore = calculateMaxRemaining currentS allPossibleAnswers candidateG
    in if isBetterGuess currentBestG currentMinMaxScore candidateG candidateMaxScore currentS
       then findBestGuessRecursive currentS allPossibleAnswers remainingCandidates candidateG candidateMaxScore
       else findBestGuessRecursive currentS allPossibleAnswers remainingCandidates currentBestG currentMinMaxScore

-- Finds the best next guess according to Knuth's Minimax algorithm.
-- This function aims to minimize the maximum number of remaining possibilities ('currentS')
-- for any possible feedback from a candidate guess.
-- It initializes the process and then calls a recursive helper to find the optimal guess.
findBestGuess :: Int -> [Row] -> [Row] -> [Answer] -> Row
findBestGuess size allCodes currentS allPossibleAnswers =
    let initialBestGuess = head allCodes
        initialMinMaxScore = calculateMaxRemaining currentS allPossibleAnswers initialBestGuess
    in findBestGuessRecursive currentS allPossibleAnswers (tail allCodes) initialBestGuess initialMinMaxScore


-- The Mastermind player function implementing Knuth's Minimax algorithm.
-- This is the core function to be used in the 'mmind' game loop.
player :: Int -> Colors -> Board -> IO Row
player size colorsList board = do
    catch
      (do
          -- Generate all possible codes once
          let allCodes = allPossibleCodes size colorsList
          -- Filter the set of possible secret codes based on the current board history
          let currentS = filterPossibleCodes allCodes board
          -- Pre-calculate all possible (black, white) feedback combinations
          let allPlausibleAnswers = possibleAnswers size

          if null board
              then -- This is the first guess of the game.
                   -- Knuth suggests a fixed optimal first guess, typically "AABB".
                   -- For size 4 and the given colors, "blue blue red red" is a commonly
                   -- accepted excellent first guess that minimizes the worst-case scenario.
                  if size >= 4 && "blue" `elem` colorsList && "red" `elem` colorsList
                      then return ["blue", "blue", "red", "red"]
                      else do
                          -- Fallback for smaller sizes or different color sets:
                          -- Just cycle through the available colors to create a first guess.
                          let defaultFirstGuess = take size (cycle colorsList)
                          return defaultFirstGuess
              else do
                  -- If 'currentS' contains only one element, that must be the secret code.
                  -- Guess it immediately to win.
                  if length currentS == 1
                      then return (head currentS)
                      else do
                          -- Otherwise, use the Minimax algorithm to find the next best guess.
                          let nextGuess = findBestGuess size allCodes currentS allPlausibleAnswers
                          return nextGuess
      )
      (\e -> do
          putStrLn $ "An error occurred in player function: " ++ show (e :: SomeException)
          putStrLn "Falling back to a random guess."
          randomRowIO size colorsList -- Fallback to a random guess in case of an error
      )


-- Defines the game to be played using the new 'player' function
play_mm = mmind 4 colors validGuess answerIs player

main = play_mm