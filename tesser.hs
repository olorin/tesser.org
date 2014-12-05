{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import           Hakyll

main :: IO ()
main = hakyllWith cfg $ do
    match "images/*" $ do
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

    match "templates/*" $ compile templateCompiler
  where
    cfg = defaultConfiguration {
        deployCommand = "rsync -avz ./_site/ tesser@tesser.wired:~/tesser.org"
    }

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext
