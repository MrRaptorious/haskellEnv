module Main (main) where

import System.Random
import Data.List

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


play_mm = mmind 4 colors validGuess answerIs naive_player

main = play_mm