import System.IO.Error (tryIOError)
import Text.Read (readMaybe)


{- 
                    Useful Info:
- link to the lab: https://chalmers.instructure.com/courses/31952/assignments/97231
- to run the game it is done through the main function. use : runhaskell Lab5.hs

                    TO DO:
- implement the QA tree data type
- implement a default tree
- implement the game
  - If QA represents the datatype for your decision trees, 
  then we recommend that the main game loop is implemented using recursion, 
  together with a function play :: QA -> IO QA 
  which implements one round of the game.

-}


-- QA decision tree type:
data QA = Answer String | Question String QA QA -- either a person (answer) or a question
  deriving (Show, Read)

-- Default decision tree
defaultTree :: QA
defaultTree = Question "Is this person from Europe?"
                (Question "Is this person a scientist?"
                    (Answer "Marie Curie")
                    (Answer "Queen Elisabeth II"))
                (Answer "Barack Obama")





-- Load the tree:
loadTree :: FilePath -> IO QA
loadTree file = do
  result <- tryIOError (readFile file)
  case result of
    Left _ -> return defaultTree -- an error occured 
    Right content -> case readMaybe content of -- using readMaybe instead of read, see the doc:
    {- 
       The read function reads input from a string, 
       which must be completely consumed by the input process. 
       read fails with an error if the parse is unsuccessful, 
       and it is therefore discouraged from being used in real applications. 
       Use readMaybe or readEither for safe alternatives.
    -}
      Just tree -> return tree
      Nothing -> return defaultTree

handlingUserInput :: String -> IO String
handlingUserInput input | input == "yes" = return "yes"
                        | input == "no" = return "no"
                        | otherwise = do 
                            putStr "Please answer yes or no! "
                            newInput <- getLine
                            handlingUserInput newInput

-- playing the game
playGame :: QA -> IO QA
playGame tree = do
    newTree <- play tree
    putStr "Play again ? "
    playingAgain <- getLine
    playingAgain <- handlingUserInput playingAgain
    if playingAgain == "yes"
        then 
        playGame newTree -- no need to return since playGame is already IO
    else
        return newTree


-- playing a game
play :: QA -> IO QA
play (Answer answer) = do
    putStr $ "My guess: Is it " ++ answer ++ "? "
    userInput <- getLine
    userInput <- handlingUserInput userInput
    if userInput == "yes"
        then do 
            putStrLn "Hurray! I won!"
            return (Answer answer)        
    else do
      putStrLn "OK - you won this time."
      putStr "Just curious: Who was your famous person? "
      newPerson <- getLine
      putStrLn $ "Give me a question for which the answer for " ++ newPerson ++ " is \"yes\" and the answer for " ++ answer ++ " is \"no\"."
      newQuestion <- getLine
      return (Question newQuestion (Answer newPerson) (Answer answer))

play (Question question yesTree noTree) = do
    putStr $ question ++ " "
    userInput <- getLine
    userInput <- handlingUserInput userInput
    if userInput == "yes"
        then do 
            update <- play yesTree  
            return (Question question update noTree)  
    else do
        update <- play noTree 
        return (Question question yesTree update) 



main :: IO()
main = do
    tree <- loadTree "questions.qa"
    afterGameTree <- playGame tree
    putStrLn "Saving QA file..."
    writeFile "questions.qa" (show afterGameTree)
    putStrLn "Bye!"