import System.Random
import Data.List
import Control.Exception (catch, SomeException)

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
-- Helper for comparing scores
compareScores :: (a, Int) -> (a, Int) -> Ordering
compareScores (_,s1) (_, s2) = compare s1 s2

-- Generates all possible code combinations
generateAllCodes :: Int -> Colors -> [Row]
generateAllCodes n availableColors = sequence (replicate n availableColors)

-- Filters a list of codes based on a guess and its answer
pruneCodes :: Row -> Answer -> [Row] -> [Row]
pruneCodes currentGuess feedback possibleCodes =
  filter (\code -> answerIs currentGuess code == feedback) possibleCodes

-- Calculates the "score" of a single guess based on the worst-case scenario
-- A lower score is better, indicating fewer remaining possible codes.
evaluateGuessPotential :: Int -> [Row] -> Row -> Int
evaluateGuessPotential codeLength remainingCodes currentGuess =
  let
    -- Generates all possible answer combinations for a given game size.
    -- This ensures we cover all (black, white) peg possibilities.
    generateAllAnswers :: Int -> [Answer]
    generateAllAnswers size =
      [ (b, w) | b <- [0..size]
               , w <- [0..size]
               , b + w <= size
               , b >= 0, w >= 0
      ]

    allPossibleAnswers = generateAllAnswers codeLength

    -- For each possible answer, determine how many codes from `remainingCodes`
    -- would produce that answer if `currentGuess` were the actual hidden code.
    calculateAnswerCounts :: Answer -> Int
    calculateAnswerCounts ans =
      length (filter (\code -> answerIs currentGuess code == ans) remainingCodes)

    -- Create a list of counts for each possible answer
    answerCounts = map calculateAnswerCounts allPossibleAnswers
  in
    -- The score of the guess is the maximum count among all possible answers.
    -- This represents the worst-case number of remaining codes.
    maximum (0:answerCounts)

-- Determines the next optimal guess based on the MinMax algorithm.
-- It finds the guess that minimizes the maximum number of remaining possible codes.
selectOptimalGuess :: Int -> Colors -> [Row] -> Row
selectOptimalGuess codeLength availableColors currentPossibleCodes =
  let
    -- Generate all possible guesses (not just the current possible codes)
    -- This helps find a guess that might not be in `currentPossibleCodes` but
    -- is strategically good for narrowing down options.
    allCandidateGuesses = generateAllCodes codeLength availableColors

    -- Calculate the score for each candidate guess
    scoredGuesses = [ (g, evaluateGuessPotential codeLength currentPossibleCodes g)
                    | g <- allCandidateGuesses
                    ]

    -- Find the guess with the minimum (best) score
    -- `minimumBy` with `compareScores` ensures we get the guess associated with the lowest score.
    bestMove = fst (minimumBy compareScores scoredGuesses)
  in
    bestMove

-- The "smart" player's logic to determine the next guess.
-- It handles error cases and applies the core MinMax strategy.
smartPlayerLogic :: Int -> Colors -> Board -> IO Row
smartPlayerLogic size colors gameBoard = do
  catch
    (do
      -- Start with all possible codes for the given game size and colors.
      let initialCodeSet = generateAllCodes size colors

      -- Refine the set of possible codes based on the history of guesses and answers.
      -- This uses a fold to sequentially apply filtering for each turn on the board.
      let refinedCodeSet = foldl (\codes (prevGuess, feedback) -> pruneCodes prevGuess feedback codes)
                                 initialCodeSet
                                 gameBoard

      -- Decide the next move:
      -- If only one code remains, that's the solution.
      -- Otherwise, use the MinMax strategy to find the optimal guess.
      case refinedCodeSet of
        [singleCode] -> return singleCode
        _            -> return $ selectOptimalGuess size colors refinedCodeSet
    )
    (\e -> do
        putStrLn $ "An error occurred in smartPlayerLogic: " ++ show (e :: SomeException)
        putStrLn "Falling back to a random guess."
        -- Fallback to a random guess in case of an error
        randomRowIO size colors
    )

-- Renamed player function to reflect the "smart" strategy.
player :: Int -> Colors -> Board -> IO Row
player = smartPlayerLogic


----------------------------------------------------
--        End of own Implementation               --
----------------------------------------------------

play_mm = mmind gameSize colors validGuess answerIs player

main = play_mm