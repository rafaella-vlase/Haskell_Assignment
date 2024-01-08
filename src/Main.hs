module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB (SnippetDB, empty, save, modify, findFirst, load, findAll, insertWith, LoadDBError(CorruptedFile))
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude


usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
  let newDB = DB.empty
  rez <- DB.save newDB
  case rez of
    Success _ -> putStrLn "Initialize new DB"
    Error err -> putStrLn $ "Error when initializing DB " ++ show err

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  db <- DB.load
  case db of
    (Error r) -> putStrLn "Failed to load DB"
    (Success db') -> do
      let ent = DB.findFirst (\x -> entryId x == getOptId getOpts) db'
      case ent of
        Nothing -> putStrLn "error"
        Just entt -> putStrLn (entrySnippet entt)

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  db <- DB.load
  case db of
    (Error r) -> putStrLn "Failed to load DB"
    (Success db') -> do
      let mat = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) db'
      case mat of
        [] -> putStrLn "No entries found"
        _ -> putStrLn (foldl (++) "" (map (\x -> show (FmtEntry x) ++ "\n") mat))

-- | Handle the add command
addOptsToEntry :: Int -> String -> AddOptions -> Entry
addOptsToEntry id snippet addOpts =
  Entry 
  { entryId = id
  , entrySnippet = snippet
  , entryFilename = addOptFilename addOpts
  , entryLanguage = addOptLanguage addOpts
  , entryDescription = addOptDescription addOpts
  , entryTags = addOptTags addOpts
  }
-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  db <- DB.load
  entries <- readFile (addOptFilename addOpts)
  case db of
    (Error r) -> putStrLn "Failed to load DB"
    (Success db') -> do
      case  DB.findFirst (\x -> entrySnippet x == entries) db' of
        Just id -> putStrLn ("Entry with this content already exists: \n" ++ show (FmtEntry id))
        Nothing -> do
          DB.modify
            ( DB.insertWith
                ( \id ->
                    Entry
                      { entryId = id,
                        entrySnippet = entries,
                        entryFilename = addOptFilename addOpts,
                        entryLanguage = addOptLanguage addOpts,
                        entryDescription = addOptDescription addOpts,
                        entryTags = addOptTags addOpts
                      }
                )
            )
          return ()


-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
