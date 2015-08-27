{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad

import           Data.Monoid

import           Hakyll

main :: IO ()
main = hakyllWith cfg $ do
    tags <- buildTags "doc/posts/*" (fromCapture "tags/*.html")
    tagsRules tags renderTagPage

    mapM_ matchStatic [
        "**/img/*"
       ,"var/*"
       ,"doc/slides/*/index.html"
       ,"**/*.pdf"
       ]

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["index.md", "doc/slides/index.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/main.html" defaultContext
            >>= relativizeUrls

    match "doc/posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags)
            >>= saveSnapshot "post_content"
            >>= loadAndApplyTemplate "templates/main.html" (tagsCtx tags)
            >>= relativizeUrls

    match "doc/index.html" $ do
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
        deployCommand = "rsync -avz ./_site/ tesser@tesser.www:~/tesser.org"
    }

    feedCtx = postCtx <> bodyField "description"

    feedContent renderFeed = do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "doc/posts/*" "post_content"
            renderFeed feedCfg feedCtx posts

    matchStatic = flip match copyStatic

    copyStatic = route idRoute >> compile copyFileCompiler

renderTagPage :: String -> Pattern -> Rules ()
renderTagPage tag pattern = do
    let title = "Pages tagged \"" <> tag <> "\""
    route idRoute
    compile $ do
        docs <- loadAll pattern >>= recentFirst
        let ctx =    constField "title" title
                  <> listField "pages" postCtx (return docs)
                  <> defaultContext
        makeItem "" >>= loadAndApplyTemplate "templates/tag.html" ctx
                    >>= loadAndApplyTemplate "templates/main.html" ctx
                    >>= relativizeUrls

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
