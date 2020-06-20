--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

import           Hakyll
import qualified Data.Text                     as T
import           System.Environment             ( lookupEnv )
import           Data.Function                  ( on )
--------------------------------------------------------------------------------

(//) :: Int -> Int -> Float
(//) = (/) `on` fromIntegral

ertField :: String -> Snapshot -> Context String
ertField name snapshot = field name $ \item -> do
  body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
  let words = length (T.words . T.pack $ body)
  return $ show $ words // 300

postCtx :: Context String
postCtx =
  field "size" (return . show . (1024 `div`) . length . itemBody)
    <> ertField "ert" "posts-content"
    <> dateField "date" "%B %e, %Y"
    <> defaultContext

--------------------------------------------------------------------------------

main :: IO ()
main = do
  compilerEnv <- lookupEnv "HAKYLL_ENV"
  let isDevelopment = compilerEnv == Just "development"

  hakyllWith defaultConfiguration $ do

    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
      route $ setExtension "html"
      compile
        $   pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    matchMetadata
        "posts/**.md"
        (\m -> isDevelopment || lookupString "status" m == Just "published")
      $ do
          route $ setExtension "html"
          compile
            $   pandocCompiler
            >>= saveSnapshot "posts-content"
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= saveSnapshot "posts-rendered"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
              listField "posts" postCtx (return posts)
                `mappend` constField "title" "Archives"
                `mappend` defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls


    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts) `mappend` defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
