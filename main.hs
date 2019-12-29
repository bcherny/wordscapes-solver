{-#LANGUAGE ScopedTypeVariables#-}

import Control.Monad
import Data.List
import Data.List.Utils
import qualified Data.Set as Set
import System.IO

getCandidates :: Int -> String -> [String]
getCandidates letterCount letters =
  uniq $ (permutations letters) >>= ((filter (\a -> length a == letterCount)) . subsequences)

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

main = do

  dictionary <- loadDictionary "./wiktionary.txt"

  putStrLn "letters?"
  letters <- getLine

  putStrLn "# of characters?"
  letterCount :: Int <- readLn

  let candidates = Set.fromList $ getCandidates letterCount letters

  websterMatches <- getMatches "./dictionary.txt" candidates
  wiktionaryMatches <- getMatches "./wiktionary.txt" candidates

  let both = Set.intersection websterMatches wiktionaryMatches
      wiki = Set.difference wiktionaryMatches websterMatches
      websters = Set.difference websterMatches wiktionaryMatches

  putStrLn $ "\nWords from both dictionaries:\n" ++ (prettyPrint both)
  putStrLn $ "Words from Webster:\n" ++ (prettyPrint websters)
  putStrLn $ "Words from Wiktionary:\n" ++ (prettyPrint wiki)
