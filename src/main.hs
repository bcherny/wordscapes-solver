{-#LANGUAGE ParallelListComp #-}
{-#LANGUAGE ScopedTypeVariables#-}

import Control.Monad
import Data.List
import Data.List.Utils
import qualified Data.Set as Set
import System.IO

data Pattern = Wildcard | Letter Char

matchesPattern :: [Pattern] -> String -> Bool
matchesPattern patterns letters
  | length patterns /= length letters = False
  | otherwise = and [matchesPatternOfSameLength p l | p <- patterns
                                                    | l <- letters
                                                    ]

matchesPatternOfSameLength :: Pattern -> Char -> Bool
matchesPatternOfSameLength Wildcard _ = True
matchesPatternOfSameLength (Letter a) b = a == b

getCandidates :: [Pattern] -> String -> [String]
getCandidates letterPattern letters =
  uniq $ (permutations letters) >>= ((filter (matchesPattern letterPattern)) . subsequences)

loadDictionary :: String -> IO (Set.Set String)
loadDictionary filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ Set.fromList $ words contents

getMatches :: String -> Set.Set String -> IO (Set.Set String)
getMatches filename candidates = do
  dictionary <- loadDictionary filename
  return $ Set.intersection dictionary candidates

prettyPrint :: (Set.Set String) -> String
prettyPrint set = foldr (\a b -> "  " ++ a ++ "\n" ++ b) "" (sort $ Set.toList set)

parseLetterPattern :: String -> [Pattern]
parseLetterPattern string =
  parseLetter <$> filter (\c -> c /= ' ') string

parseLetter :: Char -> Pattern
parseLetter '.' = Wildcard
parseLetter a = Letter a

main = do

  putStrLn "letters?"
  letters <- getLine

  putStrLn "character pattern (eg. . . e . .)?"
  letterPattern <- parseLetterPattern <$> getLine

  let candidates = Set.fromList $ getCandidates letterPattern letters

  topMatches <- getMatches "./top-full.txt" candidates
  websterMatches <- getMatches "./dictionary.txt" candidates
  wiktionaryMatches <- getMatches "./wiktionary.txt" candidates

  let likely = Set.intersection topMatches websterMatches
      top = Set.difference topMatches websterMatches
      wiki = Set.difference wiktionaryMatches websterMatches
      websters = Set.difference websterMatches wiktionaryMatches

  putStrLn $ "\nLikely words:\n" ++ (prettyPrint likely)
  putStrLn $ "Less likely words:\n" ++ (prettyPrint (websters <> wiki <> top))
