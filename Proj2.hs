-- Author: Aditi Basu (ID: 1178282) <aditib@student.unimelb.edu.au>
-- Purpose: To implement the roles of hider and searcher of a battleship-like game.

{- 
The purpose of this file is to implement the hider and searcher roles of a battleship-like game. The hider selects a set of three locations (using instances of Location type) as targets that the searcher has to attempt to find by making an initial guess (via the function initialGuess) and subsequent guesses (via the function nextGuess) using the game state (defined by GameState instances). The hider then provides feedback to the searcher (via the feedback function) that tells the latter how close their guesses are to a target. For ease of interacting with the program, toLocation and fromLocation functions have also been defined (see supporting documentation below).
-}

module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Map (fromListWith, toList)


data Location = Location Column Row 
    deriving (Eq, Show)

data Column = A | B | C | D | E | F | G | H 
    deriving (Eq, Enum, Show)

data Row = R1 | R2 | R3 | R4 
    deriving (Eq, Enum, Show)


type Guess = [Location]

type Feedback = (Int, Int, Int)

type GameState = [[Location]]

{-
Converts a string representation of a Location to a Location if it's a valid location.
Arguments:
- String representation of location
Output:
Location instance or Nothing
-}
toLocation :: String -> Maybe Location
toLocation [] = Nothing
toLocation (_x:[]) = Nothing
toLocation (x:x1:[]) = Just (Location (stringToColumn [x]) $ toEnum $ read [x1] - 1)
toLocation (_:_) = Nothing


{-
Converts a string representation of a Column to a Column instance.
Arguments:
- String representation of a Column
Output:
- Column
-}
stringToColumn :: String -> Column
stringToColumn str 
    | str == "A" = A
    | str == "B" = B
    | str == "C" = C
    | str == "D" = D
    | str == "E" = E
    | str == "F" = F
    | str == "G" = G
    | str == "H" = H
    | otherwise = error "Invalid Column"


{-
Converts a Location instance into its string representation.
Arguments:
- Location instance
Output:
- String representation of Location
-}
fromLocation :: Location -> String
fromLocation (Location col row) = (show col) ++ (show (fromEnum row + 1))


{- 
Provides feedback to the searcher about how close their guesses are to any of the 3 targets. 
Arguments: 
- List of Location instances (or Guess)
- Another list of Location instances (targets)
Output:
Feedback (Int, Int, Int) which consists of how many guesses are on a target, how many guesses are 1 space away from a target, and how many guesses are 2 spaces away from a target. Only the closest distance of a guess to a target is included (see `closestDistanceToTarget` function).
-}
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback _ [] = error "valid guess is required"
feedback [] _ = error "valid target is required"
feedback target@(_:_:_:[]) guess@(_:_:_:[]) = distanceToFeedback $ closestDistanceToTarget $ map distance [(g, t) | g <- guess, t <- target]
feedback _target _guess = error "valid target and guess are required" 


{-
Calculates the distance between a guess and a target.
Arguments:
- Tuple of (guess location, target location). Note that the this order of guess and target must be satisfied in order for the function to work accurately.
Output:
Distance (Int) from guess to target
-}
distance :: (Location, Location) -> Int
distance (Location c1 r1, Location c2 r2) = max (abs (fromEnum c1 - fromEnum c2)) (abs (fromEnum r1 - fromEnum r2))


{-
This function finds the closest distance of a guess to a target (eg. where a guess is 1 space away from 1 target and 2 spaces away from another target, the closest distance would compute as 1.
Arguments:
- List of distances of all guesses to all targets
Output:
- List of closest distances of all guesses to targets. 
-}
closestDistanceToTarget :: Ord a => [a] -> [a]
closestDistanceToTarget [] = []
closestDistanceToTarget _dist@(d1:d2:d3:ds) = [foldl min d1 (d2:[d3])] ++ closestDistanceToTarget ds
closestDistanceToTarget _ = error "list length should be a multiple of 3"


{-
Converts a set of distances to the feedback required by the `feedback` function.
Arguments: 
- List of distances of targets to guesses produced by the `distance` function
Output: 
Feedback (Int, Int, Int) - see `feedback` function output for details.
-}
distanceToFeedback :: (Eq a, Num a) => [a] -> (Int, Int, Int)
distanceToFeedback dist@(_:_:_:[]) = (length (filter (==0) dist), length (filter (==1) dist), length (filter (==2) dist))
distanceToFeedback _ = error "list should contain 3 elements"


{-
Makes the initial guess in the game. The first guess is chosen as shown because it is the guess that minimises the expected number of possible targets for the next round of guessing. This was computed brute force by using the `selectBestGuess` with `initialPossibleTargets` as its argument.
Arguments: None.
Output: 
- A tuple of (first set of guess locations, initial game state). The game state is of GameState type and consists of all the initial possible targets (computed by the `initialPossibleTargets` function)
-}
initialGuess :: ([Location], GameState)
initialGuess = (firstGuess, initialPossibleTargets) where
    firstGuess = [Location A R4, Location H R2, Location H R4]


{-
Generates all initial possible targets (sets of 3 locations) of the game. Note that while the permutations of the targets are unique, the combinations are not (eg. both [H1, A2, A3] and [A2, H1, A3] will be produced by this function). However, this does not affect the quality of the guess.
Arguments: None.
Output:
- GameState instance containing all possible targets.
-}
initialPossibleTargets :: GameState
initialPossibleTargets = [[loc1, loc2, loc3] | loc1 <- boardLocations, loc2 <- boardLocations, loc3 <- boardLocations, loc1 /= loc3, loc2 /= loc1, loc2 /= loc3]


{-
Generates all the possible board locations.
Arguments: None.
Output:
- List of all Locations on the game board
-}
boardLocations :: [Location]
boardLocations = do
    col <- [A, B, C, D, E, F, G, H]
    row <- [R1, R2, R3, R4]
    return $ Location col row


{-
Generates every subsequent guess using the last Guess, its corresponding Feedback and the GameState
Arguments:
- A tuple of the last Guess and GameState
- The last Guess's corresponding Feedback
Output:
- A tuple of the next Guess and new GameState
-}
nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> ([Location], GameState)
nextGuess (locs, gs) fb = (head $ new_gs, new_gs)
    where new_gs = removeTargets gs locs fb


{-
Removes all targets from the GameState that are not consistent with the Feedback received for the last Guess. 
Arguments:
- Previous GameState
- Last Guess
- Corresponding Feedback to Guess
Output:
- New GameState
Assumptions:
- Each set of targets and Guess are of length 3.
- Feedback is valid.
-}
removeTargets :: GameState -> Guess -> Feedback -> GameState
removeTargets targets@(_:_) guess fb = filter (\x -> feedback x guess == fb) targets 
removeTargets [] _ _ = [] 

{-
The following functions are not being used in the current program, but have been used to calculate the best first guess, as seen in the `initialGuess` function.
-}

{-
Selects the best Guess given a set of possible targets by calculating the number of expected remaining targets for each guess and selecting the guess that has the minimum number.
Arguments: 
- Current GameState
Output:
- Best Guess
-}
selectBestGuess :: GameState -> Guess
selectBestGuess [] = error "no guesses left!"
selectBestGuess (g:[]) = g
selectBestGuess gamestate = minGuess (\x -> remainingCandidates $ map snd $ frequencies $ feedbacksPerGuess x gamestate) gamestate


{-
Calculates the Feedback for each possible Guess and target given a GameState.
Arguments:
- Guess
- GameState
Output: 
- List of Feedbacks for specified Guess against all possible targets
-}
feedbacksPerGuess :: Guess -> GameState -> [Feedback]
feedbacksPerGuess _ [] = []
feedbacksPerGuess guess targets = map (\t -> feedback t guess) targets


{-
Calculates the frequency of each type of Feedback for a single Guess. Adapted from https://stackoverflow.com/questions/10398698/haskell-counting-how-many-times-each-distinct-element-in-a-list-occurs)
Arguments:
- List of Feedbacks
Output:
- List of (Feedback, Feedback frequency) tuples
-}
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies feedbacks = toList (fromListWith (+) [(f, 1) | f <- feedbacks])


{-
Calculates the expected number of remaining candidates (targets) given a Guess's Feedback frequencies as computed by the `frequencies` function.
Arguments:
- List of Feedback counts
Output: 
- Number of expected remaining candidates
-}
remainingCandidates :: (Fractional a) => [Int] -> a
remainingCandidates feedbackCounts = fromIntegral (sum (map (^2) feedbackCounts)) / fromIntegral (sum feedbackCounts)


{-
Computes the element in a list that obtains the minimum value of a specified function. Adapted from https://stackoverflow.com/questions/70009007/haskell-function-to-get-minimum-of-a-list-depending-on-a-given-function
Arguments:
- A function
- A list
Output:
- The element in the list producing the minimum value of the function
-}
minGuess :: (Ord b) => (a -> b) -> [a] -> a
minGuess func list =
    case list of 
        [] -> error "function needs to have arguments"
        [x] -> x
        x1:x2:xs -> 
            if func x1 < func x2
                then minGuess func (x1:xs)
                else minGuess func (x2:xs)

