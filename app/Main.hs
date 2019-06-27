module Main where

import           Data.Foldable
import qualified Data.Text                  as T
import           Development.Shake
import           Development.Shake.FilePath
import           Slick

-- convert a source filepath to a build filepath
-- e.g. site/css/style.css -> build/css/style.css
srcToBuild :: FilePath -> FilePath
srcToBuild path = "build" </> dropDirectory1 path

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
      staticFiles <- getDirectoryFiles "site" ["css//*", "js//*", "images//*"]
      let copyStaticFile path = copyFileChanged path (srcToBuild path)
      traverse_ copyStaticFile staticFiles

     -- Find and require every post to be  built
     -- This uses the `~>` 'phony' rule because it doesn't actually write any
     -- files on its own
    "posts" ~> do
      postPaths <- getDirectoryFiles "site/posts" ["*.md"]

      -- We tell shake we need to build each individual post
      -- We require each post separately so that Shake can cache them
      -- individually
      need (((-<.> "html") . srcToBuild) <$> postPaths)

     -- rule for actually building posts
    "build/posts//*.html" %> \out -> do

      -- Recover the path where the source file for the post should be
      let srcPath = (dropDirectory1 out) -<.> "md"
      fileContents <- readFile' srcPath

      -- Load a markdown source file into an Aeson Value
      -- The 'content' key contains an html-rendered string
      -- Any metadata from a yaml block is loaded into the appropriate keys in
      -- the Aeson object, e.g. author, date, tags, etc.
      postData <- markdownToHTML . T.pack $ fileContents

      -- Load a mustache template using using cache if available
      template <- compileTemplate' "site/templates/post.html"

      -- Fill in the template using the post metadata/content
      writeFile' out . T.unpack $ substitute template postData
