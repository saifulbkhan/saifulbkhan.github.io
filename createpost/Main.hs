{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where
import           Data.Aeson                 as A
import           Data.Yaml                  as Y
import           Data.ByteString.UTF8       as B
import           Data.Time
import           Data.Char
import           GHC.Generics               (Generic)
import           System.Environment
import           System.FilePath

-- | A JSON serializable representation of a post's metadata
data PostFile =
  PostFile
    { title       :: String
    , author      :: String
    , date        :: String
    , tags        :: [String]
    }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON PostFile

instance ToJSON PostFile

-- | Directory where all posts go
postDir :: FilePath
postDir = "site" </> "posts"

-- | That would be me
defaultAuthor :: String
defaultAuthor = "Saiful B. Khan"

main :: IO ()
main = do
  args <- getArgs
  (path, generated) <- parsePostArgs args
  let post = addAuthorIfNone generated
      postData = Y.encode post
  writeFile path "---\n"
  appendFile path $ B.toString postData
  appendFile path "---\n"

parsePostArgs :: [String] -> IO (FilePath, PostFile)
parsePostArgs args = do
  let hunk = PostFile { title = ""
                      , author = ""
                      , date = ""
                      , tags = []
                      }
      modded = modifyForArgs args hunk
  datetime <- getCurrentIsoTime
  let post = modded { date = datetime }
      path = getPathForPost post
  return (path, post)

modifyForArgs :: [String] -> PostFile -> PostFile
modifyForArgs [] post = post
modifyForArgs (x:xs) post =
  if (head x == '-')
    then modifyForArgs (dropVals xs) (modded post xs)
    else modifyForArgs xs post
  where
    takeVals l = takeWhile (\a -> head a /= '-') l
    dropVals l = dropWhile (\a -> head a /= '-') l
    modded p l = modifyForArg x (takeVals l) p

modifyForArg :: String -> [String] -> PostFile -> PostFile
modifyForArg "-t" ts post = addTags ts post
modifyForArg "--tags" ts post = addTags ts post
modifyForArg "-n" [t] post = addTitle t post
modifyForArg "--title" [t] post = addTitle t post
modifyForArg "-a" [a] post = addAuthor a post
modifyForArg "--author" [a] post = addAuthor a post
modifyForArg _ _ post = post

addTags :: [String] -> PostFile -> PostFile
addTags tags post = post { tags = tags }

addTitle :: String -> PostFile -> PostFile
addTitle t post = post { title = t }

addAuthor :: String -> PostFile -> PostFile
addAuthor a post = post { author = a }

getPathForPost :: PostFile -> FilePath
getPathForPost post =
  if (title post == "")
    then makeFilePath $ date post
    else makeFilePath $ map (spaceToDash .toLower) $ title post
  where
    spaceToDash ' ' = '-'
    spaceToDash x   = x
    makeFilePath filename = postDir </> filename <.> "md"

getCurrentIsoTime :: IO String
getCurrentIsoTime = do
  utct <- getCurrentTime 
  return $ (formatTime defaultTimeLocale isoFormat utct) ++ "+00:00"
  where isoFormat = iso8601DateFormat (Just "%H:%M:%S")

addAuthorIfNone :: PostFile -> PostFile
addAuthorIfNone post = case (author post) of
  "" -> post { author = defaultAuthor }
  _  -> post
