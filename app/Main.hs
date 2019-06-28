module Main where

import           Data.Foldable
import qualified Data.Text                  as T
import           Development.Shake
import           Development.Shake.FilePath
import           Slick

buildDirName :: FilePath
buildDirName = "build"

siteDirName :: FilePath
siteDirName = "site"

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

-- convert a source filepath to a build filepath
-- e.g. site/css/style.css -> build/css/style.css
srcToBuild :: FilePath -> FilePath
srcToBuild path = buildDirName </> dropDirectory1 path

main :: IO ()
main =
  shakeArgs shakeOptions $
    -- Require all the things we need to build the site
    -- For this simplified example we'll just copy static assets and build a
    -- page for each post
   do
    "site" ~> need ["static", "posts"]

    -- Require all static assets
    "static" ~> do
      let copyStaticFile path = copyFileChanged path (srcToBuild path)
      let allChildren dir = dir </> "/*"
      staticFiles <- getDirectoryFiles "." (allChildren <$> [ cssDir
                                                            , jsDir
                                                            , imgDir ])
      traverse_ copyStaticFile staticFiles

     -- Find and require every post to be  built
     -- This uses the `~>` 'phony' rule because it doesn't actually write any
     -- files on its own
    "posts" ~> do
      postPaths <- getDirectoryFiles postsDir ["*.md"]

      -- We tell shake we need to build each individual post
      -- We require each post separately so that Shake can cache them
      -- individually
      need (((-<.> "html") . srcToBuild) <$> postPaths)

     -- rule for actually building posts
    ((buildDirName </> postsDirName) ++ "//*.html") %> \out -> do

      -- Recover the path where the source file for the post should be
      let srcPath = (dropDirectory1 out) -<.> "md"
      fileContents <- readFile' srcPath

      -- Load a markdown source file into an Aeson Value
      -- The 'content' key contains an html-rendered string
      -- Any metadata from a yaml block is loaded into the appropriate keys in
      -- the Aeson object, e.g. author, date, tags, etc.
      postData <- markdownToHTML . T.pack $ fileContents

      -- Load a mustache template using using cache if available
      template <- compileTemplate' (templatesDir </> postTemplate)

      -- Fill in the template using the post metadata/content
      writeFile' out . T.unpack $ substitute template postData
