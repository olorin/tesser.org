{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Data.Maybe
import Hakyll
import System.Environment
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)

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

    match "blog/posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/blog.html" postCtx
            >>= relativizeUrls

    create ["blog/archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/blog.html" archiveCtx
                >>= relativizeUrls


    create ["blog/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "blog/posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Blog"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/blog.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
    where 
        cfg = defaultConfiguration {
            deployCommand = "rsync -avz ./_site/ tesser@tesser.wired:~/tesser.org"
        }

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
