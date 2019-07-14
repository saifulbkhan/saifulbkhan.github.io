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
import           Data.List                  (sortBy, take)
import           Data.Set                   as S
import qualified Data.Text                  as T
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

miscDirName :: FilePath
miscDirName = "misc"

tagsDirName :: FilePath
tagsDirName = "tags"

postsDir :: FilePath
postsDir = siteDirName </> postsDirName

miscDir :: FilePath
miscDir = siteDirName </> miscDirName

cssDir :: FilePath
cssDir = "css"

jsDir :: FilePath
jsDir = "js"

imgDir :: FilePath
imgDir = "images"

fontsDir :: FilePath
fontsDir = "fonts"

templatesDir :: FilePath
templatesDir = siteDirName </> "templates"

postTemplate :: FilePath
postTemplate = "post.html"

indexTemplate :: FilePath
indexTemplate = "index.html"

collectionTemplate :: FilePath
collectionTemplate = "posts.html"

aboutTemplate :: FilePath
aboutTemplate = "about.html"

main :: IO ()
main = do
  copyrightString <- getCopyrightString
  shakeArgs shakeOptions {shakeVerbosity = Chatty} $ do

  -- Set up caches
    postCache <- jsonCache' loadPost

    -- Require all the things we need to build the whole site
    siteDirName ~> need [ staticDirName
                        , postsDirName
                        , tagsDirName
                        , buildDirName </> indexTemplate
                        , buildDirName </> collectionTemplate
                        , buildDirName </> aboutTemplate
                        ]

    -- Helper function that returns a match pattern for all children in a given
    -- directory
    let allChildren dir = dir ++ "//*"

    -- Require all static assets
    staticDirName ~> do
      staticFiles <- getDirectoryFiles siteDirName $ allChildren <$> [ cssDir
                                                                     , jsDir
                                                                     , imgDir
                                                                     , fontsDir
                                                                     ]
      need ((buildDirName </>) <$> staticFiles)

    -- Rule for handling static assets, just copy them from source to dest
    allChildren <$> (buildDirName </>) <$> [cssDir, jsDir, imgDir, fontsDir] |%>
      \out -> do copyFileChanged (siteDirName </> dropDirectory1 out) out

     -- Find and require every post to be built
    postsDirName ~> requirePosts

     -- Find and require every tag page to be built
    tagsDirName ~> requireSubjects postCache tags tagsDirName

    -- build the main landing page
    buildDirName </> indexTemplate %> buildIndex postCache

    -- build the blog archive
    buildDirName </> collectionTemplate %> buildArchive postCache

    -- build the about page
    buildDirName </> aboutTemplate %> buildPage (miscDir </> "about.md")

    -- rule for actually building posts
    buildDirName </> postsDirName ++ "/*.html" %>
      buildPost postCache copyrightString

    -- rule for building required tags
    buildDirName </> tagsDirName ++ "/*.html" %> buildSubject postCache tags

-- | Represents the template dependencies of the index page
data IndexInfo =
  IndexInfo
    { posts :: [Post]
    }
  deriving (Generic, Show)

instance FromJSON IndexInfo

instance ToJSON IndexInfo

-- | Represents the template dependencies of a collection (can be useful for
-- tags, categories, topics, etc.)
data CollectionInfo =
  CollectionInfo
    { subject    :: String
    , collection :: [Post]
    }
  deriving (Generic, Show)

instance FromJSON CollectionInfo

instance ToJSON CollectionInfo

-- | A JSON serializable representation of a post's metadata
data Post =
  Post
    { title       :: String
    , author      :: String
    , content     :: String
    , url         :: String
    , date        :: String
    , copyright   :: Maybe String
    , hasTag      :: Maybe Bool
    , tags        :: Maybe [String]
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
  Just utct -> p { date = fn utct }

-- | Creates a copyright string usable at this point in time
getCopyrightString :: IO String
getCopyrightString = do
  (year, _, _) <- getCurrentTime >>= return . toGregorian . utctDay
  return $ "© 2016 - " ++ (show year) ++ " Saiful Bari Khan"

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

-- | Obtains the tags from a Post object.
tagsFromPost :: Value -> Parser (Maybe [String])
tagsFromPost = withObject "tuple" $ \o -> o .: "tags"

-- | Obtain all the given type of subjects attributed to an entire collection
-- of posts.
getSubjects :: [Post] -> (Post -> Maybe [String]) -> [String]
getSubjects posts stype = S.toList $ extractS posts S.empty
  where
    extractS []     found = found
    extractS (p:ps) found = case (stype p) of
      Nothing -> extractS ps found
      Just ss -> extractS ps $ S.union (S.fromList ss) found

-- | Filter all the posts based on the criterion of whether they are attributed
-- to a given subject.
getPostsWithSubject :: String -> [Post] -> (Post -> Maybe [String]) -> [Post]
getPostsWithSubject subject posts stype = Prelude.filter checkSubject posts
  where
    checkSubject p = case (stype p) of
      Nothing -> False
      Just ss -> elem subject ss

-- | Given a post source-file's file path as a cache key, load the Post object
-- for it. This is used with 'jsonCache' to provide post caching.
loadPost :: PostFilePath -> Action Post
loadPost (PostFilePath postPath) = do
  let srcPath = destToSrc postPath -<.> "md"
  postData <- readFile' srcPath >>= markdownToHTML . T.pack
  let postURL = T.pack . srcToURL $ postPath
      withURL = _Object . at "url" ?~ String postURL
      withSrc = _Object . at "srcPath" ?~ String (T.pack srcPath)
      maybeTags = parseMaybe tagsFromPost postData
      withTag = assignBool (not . isNothing <$> maybeTags) "hasTag"
  convert . withSrc . withURL . withTag $ postData
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
  archiveT <- compileTemplate' (templatesDir </> collectionTemplate)
  let subject = "Archive"
      sorted = reverse . sortByTime $ allPosts
      collection = (formatPostDate dateFormatFn) <$> sorted
      archiveInfo = CollectionInfo {subject, collection}
      archiveHTML = T.unpack $ substitute archiveT (toJSON archiveInfo)
  writeFile' out archiveHTML

-- | Given a file path this will create a page
buildPage :: FilePath -> FilePath -> Action ()
buildPage pageSrc out = do
  page <- loadPost $ PostFilePath pageSrc
  aboutT <- compileTemplate' (templatesDir </> aboutTemplate)
  let aboutHTML = T.unpack $ substitute aboutT (toJSON page)
  writeFile' out aboutHTML

-- | Find all subjects in source files and tell shake to build the corresponding
-- html pages.
requireSubjects :: (PostFilePath -> Action Post)
                -> (Post -> Maybe [String])
                -> FilePath
                -> Action ()
requireSubjects postCache stype dirName = do
  allPosts <- postNames >>= traverse (postCache . PostFilePath)
  let subjectNames = getSubjects allPosts stype
  need ((\s -> addExtension s "html")
        <$> (buildDirName </>)
        <$> (dirName </>)
        <$> subjectNames)

-- | Find all post source files and tell shake to build the corresponding html
-- pages.
requirePosts :: Action ()
requirePosts = do
  pNames <- postNames
  need ((\p -> srcToDest p -<.> "html") <$> pNames)

-- | Build an html file for a given post given a cache of posts.
buildPost :: (PostFilePath -> Action Post) -> String -> FilePath -> Action ()
buildPost postCache copyrightString out = do
  let srcPath = destToSrc out -<.> "md"
  loadedPost <- postCache (PostFilePath srcPath)
  let post = loadedPost {copyright = Just copyrightString}
  template <- compileTemplate' (templatesDir </> postTemplate)
  writeFile' out . T.unpack $
    substitute template (toJSON . formatPostDate dateTimeFormatFn $ post)

-- | Build an html file for a given subject given a cache of posts.
buildSubject :: (PostFilePath -> Action Post)
             -> (Post -> Maybe [String])
             -> FilePath
             -> Action ()
buildSubject postCache stype out = do
  allPosts <- postNames >>= traverse (postCache . PostFilePath)
  collectionT <- compileTemplate' (templatesDir </> collectionTemplate)
  let subject = takeBaseName out
      allPostsWithSubject = getPostsWithSubject subject allPosts stype
      collection = (formatPostDate dateFormatFn) <$> allPostsWithSubject
      subjectInfo = CollectionInfo {subject, collection}
      subjectHTML = T.unpack $ substitute collectionT (toJSON subjectInfo)
  writeFile' out subjectHTML