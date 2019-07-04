{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where
import           Control.Lens
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.Aeson.Types           (Parser, parseMaybe)
import           Data.Maybe                 (isNothing)
import           Data.Function              (on)
import           Data.List                  (sortBy, take)
import           Data.Map                   as M
import           Data.Monoid
import           Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Text.Lens
import           Data.Time
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Slick

-- | Shows this many latest post on main webpage
showLatestN :: Int
showLatestN = 3

buildDirName :: FilePath
buildDirName = "dist"

siteDirName :: FilePath
siteDirName = "site"

staticDirName :: FilePath
staticDirName = "static"

postsDirName :: FilePath
postsDirName = "posts"

postsDir :: FilePath
postsDir = siteDirName </> postsDirName

cssDir :: FilePath
cssDir = siteDirName </> "css"

jsDir :: FilePath
jsDir = siteDirName </> "js"

imgDir :: FilePath
imgDir = siteDirName </> "images"

templatesDir :: FilePath
templatesDir = siteDirName </> "templates"

postTemplate :: FilePath
postTemplate = "post.html"

indexTemplate :: FilePath
indexTemplate = "index.html"

archiveTemplate :: FilePath
archiveTemplate = "posts.html"

main :: IO ()
main =
  shakeArgs shakeOptions {shakeVerbosity = Chatty} $ do

  -- Set up caches
    postCache <- jsonCache' loadPost

    -- Require all the things we need to build the whole site
    siteDirName ~> need [ staticDirName
                        , postsDirName
                        , buildDirName </> indexTemplate
                        , buildDirName </> archiveTemplate
                        ]

    -- Helper function that returns a match pattern for all children in a given
    -- directory
    let allChildren dir = dir ++ "//*"

    -- Require all static assets
    staticDirName ~> do
      staticFiles <- getDirectoryFiles siteDirName $ allChildren <$> [ cssDir
                                                                     , jsDir
                                                                     , imgDir
                                                                     ]
      need ((buildDirName </>) . dropDirectory1 <$> staticFiles)

    -- Rule for handling static assets, just copy them from source to dest
    allChildren <$> (buildDirName </>) <$> [cssDir, jsDir, imgDir] |%> \out ->
      do copyFileChanged (siteDirName </> dropDirectory1 out) out

     -- Find and require every post to be built
    postsDirName ~> requirePosts

    -- build the main landing page
    buildDirName </> indexTemplate %> buildIndex postCache

    -- build the blog archive
    buildDirName </> archiveTemplate %> buildArchive postCache

    -- rule for actually building posts
    buildDirName </> postsDirName ++ "/*.html" %> buildPost postCache

-- | Represents the template dependencies of the index page
data IndexInfo =
  IndexInfo
    { posts :: [Post]
    }
  deriving (Generic, Show)

instance FromJSON IndexInfo

instance ToJSON IndexInfo

-- | A JSON serializable representation of a post's metadata
data Post =
  Post
    { title       :: String
    , author      :: String
    , content     :: String
    , url         :: String
    , date        :: String
    , hasTag      :: Maybe Bool
    , hasCategory :: Maybe Bool
    , tags        :: Maybe [String]
    , categories  :: Maybe [String]
    }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON Post

instance ToJSON Post

-- A simple wrapper data-type which implements 'ShakeValue';
-- Used as a Shake Cache key to build a cache of post objects.
newtype PostFilePath =
  PostFilePath String
  deriving (Show, Eq, Hashable, Binary, NFData)

-- | Discover all available post source files
postNames :: Action [FilePath]
postNames = getDirectoryFiles "." [postsDir ++ "//*.md"]

-- | convert 'build' filepaths into source file filepaths
destToSrc :: FilePath -> FilePath
destToSrc p = siteDirName </> dropDirectory1 p

-- | convert source filepaths into build filepaths
srcToDest :: FilePath -> FilePath
srcToDest p = buildDirName </> dropDirectory1 p

-- | convert a source file path into a URL
srcToURL :: FilePath -> String
srcToURL = ("/" ++) . dropDirectory1 . (-<.> ".html")

-- | Convert a date-time string to a UTCTime. It considers two different formats
-- and if neither fits, the result is Nothing.
stringToDate :: String -> Maybe UTCTime
stringToDate s =
  case timeWithSpace of
    Nothing -> fmap zonedTimeToUTC timeWithT
    _ -> fmap zonedTimeToUTC timeWithSpace
  where
    parseDt = parseTimeM True defaultTimeLocale
    timeWithSpace = parseDt "%F %T%z" s :: Maybe ZonedTime
    timeWithT = parseDt "%FT%T%z" s :: Maybe ZonedTime

-- | Given a UTCTime, outputs a String formatted as:
-- {full name of month} {day of month}, {year without padding}.
dateFormatFn :: UTCTime -> String
dateFormatFn = formatTime defaultTimeLocale "%B %d, %Y"

-- | Given a UTCTime, outputs a String formatted as:
-- {hour of day-half, 12 hour}:{minute of hour} {day-half of day},
-- {day of month} {short name of month} {year without padding} ({time zone})
dateTimeFormatFn :: UTCTime -> String
dateTimeFormatFn = formatTime defaultTimeLocale "%I:%M %p, %d %b %Y (%Z)"

-- | Given a date-time formatting function and a Post, changes the string format
-- of the "date" field of the post using the formatting function.
formatPostDate :: (UTCTime -> String) -> Post -> Post
formatPostDate fn p = case stringToDate (date p) of
  Nothing -> p
  Just utc -> p { date = fn utc }

-- | Sorts a list of posts by the date on which they were written (ascending).
sortByTime :: [Post] -> [Post]
sortByTime posts = sortBy sortFn posts
  where
    sortFn p p' =
      let t = stringToDate $ date p
          t' = stringToDate $ date p'
      in case diffUTCTime <$> t <*> t' of
        Nothing -> LT
        Just timeDiff -> compare timeDiff 0.0

-- | Obtains the tags and categories from a Post object.
tagsAndCategories :: Value -> Parser (Maybe [String], Maybe [String])
tagsAndCategories = withObject "tuple" $ \o -> do
  t <- o .: "tags"
  c <- o .: "categories"
  return (t, c)

-- | Given a post source-file's file path as a cache key, load the Post object
-- for it. This is used with 'jsonCache' to provide post caching.
loadPost :: PostFilePath -> Action Post
loadPost (PostFilePath postPath) = do
  let srcPath = destToSrc postPath -<.> "md"
  postData <- readFile' srcPath >>= markdownToHTML . T.pack
  let postURL = T.pack . srcToURL $ postPath
      withURL = _Object . at "url" ?~ String postURL
      withSrc = _Object . at "srcPath" ?~ String (T.pack srcPath)
      maybeTnC = parseMaybe tagsAndCategories postData
      withTag = assignBool (not . isNothing . fst <$> maybeTnC) "hasTag"
      withCat = assignBool (not . isNothing . snd <$> maybeTnC) "hasCategory"
  convert . withSrc . withURL . withTag . withCat $ postData
  where
    assignBool :: Maybe Bool -> T.Text -> (Value -> Value)
    assignBool (Just x) field = _Object .at field ?~ Bool x
    assignBool Nothing field = _Object .at field ?~ Bool False

-- | given a cache of posts this will create landing page with latest posts
buildIndex :: (PostFilePath -> Action Post) -> FilePath -> Action ()
buildIndex postCache out = do
  allPosts <- postNames >>= traverse (postCache . PostFilePath)
  indexT <- compileTemplate' (templatesDir </> indexTemplate)
  let latest = Data.List.take showLatestN . reverse . sortByTime $ allPosts
      posts = (formatPostDate dateFormatFn) <$> latest
      indexInfo = IndexInfo {posts}
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFile' out indexHTML

-- | given a cache of posts this will create a blog archive
buildArchive :: (PostFilePath -> Action Post) -> FilePath -> Action ()
buildArchive postCache out = do
  allPosts <- postNames >>= traverse (postCache . PostFilePath)
  archiveT <- compileTemplate' (templatesDir </> archiveTemplate)
  let sorted = reverse . sortByTime $ allPosts
      posts = (formatPostDate dateFormatFn) <$> sorted
      indexInfo = IndexInfo {posts}
      archiveHTML = T.unpack $ substitute archiveT (toJSON indexInfo)
  writeFile' out archiveHTML

-- | Find all post source files and tell shake to build the corresponding html
-- pages.
requirePosts :: Action ()
requirePosts = do
  pNames <- postNames
  need ((\p -> srcToDest p -<.> "html") <$> pNames)

-- | Build an html file for a given post given a cache of posts.
buildPost :: (PostFilePath -> Action Post) -> FilePath -> Action ()
buildPost postCache out = do
  let srcPath = destToSrc out -<.> "md"
      postURL = srcToURL srcPath
  post <- postCache (PostFilePath srcPath)
  template <- compileTemplate' (templatesDir </> postTemplate)
  writeFile' out . T.unpack $
    substitute template (toJSON . formatPostDate dateTimeFormatFn $ post)
