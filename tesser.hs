{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import           Hakyll

main :: IO ()
main = hakyllWith cfg $ do
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "var/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["index.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/main.html" defaultContext
            >>= relativizeUrls

    match "doc/posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= saveSnapshot "post_content"
            >>= loadAndApplyTemplate "templates/main.html" postCtx
            >>= relativizeUrls

    create ["doc/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "doc/posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Documents"           <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/main.html" indexCtx
                >>= relativizeUrls

    create ["rss.xml"] $ feedContent renderRss

    create ["atom.xml"] $ feedContent renderAtom

    match "templates/*" $ compile templateCompiler
  where
    cfg = defaultConfiguration {
        deployCommand = "rsync -avz ./_site/ tesser@tesser.wired:~/tesser.org"
    }

    feedCtx = postCtx <> bodyField "description"

    feedContent renderFeed = do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "doc/posts/*" "post_content"
            renderFeed feedCfg feedCtx posts

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

tagsCtx :: Tags -> Context String
tagsCtx tags =
    tagsField "tags" tags <> postCtx

feedCfg :: FeedConfiguration
feedCfg = FeedConfiguration {
      feedTitle = "tesser.org - posts"
    , feedDescription = "posts on tesser.org"
    , feedAuthorName = "Sharif Olorin"
    , feedAuthorEmail = "sio@tesser.org"
    , feedRoot = "https://tesser.org"
}
