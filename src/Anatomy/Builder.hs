module Anatomy.Builder where

import Control.Monad.State
import Data.Char
import Data.Dynamic
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import System.Directory
import System.FilePath
import Text.HTML.TagSoup

import {-# SOURCE #-} qualified Anatomy.Atomo as A
import Anatomy.AutoFlow
import Anatomy.Parser
import Anatomy.Scanner
import Anatomy.Types

import Atomo.Environment
import Atomo.Run
import Atomo.Types
import Atomo.Parser.Expand (macroExpand)

import Paths_anatomy


build :: Segment -> AVM String
build (Chunk s) = return s
build (KeywordDispatch ns ss) = do
    vs <- forM ss $ \s ->
        case s of
            Atomo e -> return (Expression e)
            _ -> fmap string (build s)

    a <- getAObject
    liftM (fromText . fromString) $ lift (dispatch (keyword ns (a:vs)))
build (SingleDispatch n) = do
    s <- getAObject
    res <- lift (dispatch (single n s))
    return (fromText $ fromString res)
build (Atomo e) = do
    env <- gets sectionA >>= lift . dispatch . single "environment"
    lift (macroExpand e >>= withTop env . eval)
    return ""
build (Nested ss) = fmap concat $ mapM build ss
build (SectionReference n) = do
    style <- gets sectionStyle
    if styleMatch TOC style
        then return ""
        else do

    sec <- gets ((!! n) . subSections)
    flip runAVM sec $ do
        depth <- do
            myd <- gets sectionDepth
            mp <- gets sectionParent
            case mp of
                Nothing -> return myd
                Just p -> do
                    pd <- fmap sectionDepth $ liftIO (readIORef p)
                    return (myd - pd)

        u <- get >>= buildForString' . tagOrTitle
        t <- gets (titleText . sectionTitle) >>= buildForString'
        b <- mapM build (sectionBody sec)

        let header = 'h' : show (depth + 1)
        return . unlines $
            [ "<div class=\"section\">"
            , "  " ++ concat
                [ "<" ++ header ++ " class=\"section-header\" id=\"section_"
                    ++ sanitize u ++ "\">"
                , t
                , "</" ++ header ++ ">"
                ]
            , "  " ++ concat b
            , "</div>"
            ]
build TableOfContents =
    liftM printTOC (get >>= buildTOC)
build FullTableOfContents =
    liftM printFullTOC (get >>= buildTOC)
build (InlineDefinition d b) = do
    a <- getAObject

    thumb <- liftM (fromText . fromString) . lift $
        dispatch (keyword ["pretty"] [a, Expression (defThumb d)])

    pr <- liftM (fromText . fromString) . lift $
        dispatch (keyword ["pretty"] [a, Expression (defReturn d)])

    pcs <- lift $ mapM (\c -> liftM (fromText . fromString) $
        dispatch (keyword ["pretty"] [a, Expression c])) (defContracts d)

    body <- maybe (return "") (fmap (++ "\n\n") . build) b
    return . unlines $
        [ "<div class=\"definition\" id=\"" ++ bindingID (defKey d) ++ "\">"
        , "  <pre class=\"thumb\">" ++ concat
            [ thumb
            , " <span class=\"definition-result-arrow\">&rarr;</span> "
            , pr
            , concatMap ("\n  | " ++) pcs
            ] ++ "</pre>"
        , body
        , ""
        , "</div>"
        ]

buildTOC :: Section -> AVM TOCTree
buildTOC s
    | null (subSections s) = do
        u <- sectionURL s
        fmap (Node u) $ build (titleText (sectionTitle s))
    | otherwise = do
        t <- build (titleText (sectionTitle s))
        u <- sectionURL s
        ts <- mapM buildTOC (subSections s)
        return (Branch u t ts)

-- print a table of contents; skip the first node if it's
-- a branch (since we should already be on that page)
printTOC :: TOCTree -> String
printTOC t =
    case t of
        Branch _ _ ss ->
            "<ol class=\"toc\">" ++ concatMap printTOC' ss ++ "</ol>"
        _ ->
            "<ol class=\"toc\">" ++ printTOC' t ++ "</ol>"

printFullTOC :: TOCTree -> String
printFullTOC t = "<ol class=\"toc\">" ++ printTOC' t ++ "</ol>"

printTOC' :: TOCTree -> String
printTOC' (Node u n) =
    "<li><a href=\"" ++ u ++ "\">" ++ n ++ "</a></li>"
printTOC' (Branch u n ss) =
    "<li><a href=\"" ++ u ++ "\">" ++ n ++ "</a><ol>"
        ++ concatMap printTOC' ss ++ "</ol></li>"

buildFile :: FilePath -> FilePath -> IO ()
buildFile fn o = do
    createDirectoryIfMissing True o

    forM_
        [ "anatomy.css"
        , "highlight.css"
        , "jquery.js"
        , "jquery.hotkeys.js"
        , "main.js"
        ] $ \l ->
        getDataFileName ("lib/" ++ l) >>=
            readFile >>= writeFile (o </> l)

    path <- fmap takeDirectory $ canonicalizePath fn

    exec $ do
        A.load

        ast <- parseFile fn

        start <- scan 0 1 path ast

        liftIO (putStrLn "scanned document")

        runAVM' (buildDocument o) start

        return (particle "ok")

buildDocument :: FilePath -> AVM Value
buildDocument o = do
    liftIO (putStrLn ("building document to: " ++ o))

    s <- get

    liftIO . print . titleText $ sectionTitle s

    when (styleMatch TOC $ sectionStyle s) $ do
        liftIO (putStrLn "building subsections first for table of contents")
        mapM_ (runAVM (buildDocument o)) (subSections s)

    liftIO (putStrLn "building table of contents")
    toc <- build TableOfContents

    liftIO (putStrLn "building title")
    title <- build . titleText . sectionTitle $ s

    liftIO (putStrLn "building body")
    body <- fmap concat $ forM (sectionBody s) $ \b -> do
         case b of
             SectionReference n | and
                [ not (styleMatch TOC (sectionStyle s))
                , styleMatch TOC (sectionStyle $ subSections s !! n)
                ] -> do
                let c = subSections s !! n
                runAVM (buildDocument o) c
                return ""
             _ -> build b

    liftIO (putStrLn "getting parent")
    parent <-
        case sectionParent s of
            Nothing -> return Nothing
            Just p ->
                liftM Just $ liftIO (readIORef p)
                    >>= runAVM (build FullTableOfContents)

    liftIO (putStr "writing document to...")
    fn <- sectionURL s
    liftIO (putStrLn fn)

    let showTOC = isJust parent || not (null (subSections s))
        classes | showTOC = "with-sidebar " ++ styleToClass (sectionStyle s)
                | otherwise = styleToClass (sectionStyle s)

    liftIO (putStrLn "generating search tags")
    ts <- searchTags s
    liftIO . writeFile (o </> "tags.js") . concat $
        [ "var SEARCH_TAGS = [\n  "
        , intercalate ",\n  " $
            map (\(k, n, v) -> "[" ++ show k ++ ", " ++ show n ++ ", " ++ show v ++ "]") ts
        , "\n];"
        ]

    liftIO . writeFile (o </> fn) . unlines $
        [ "<!DOCTYPE html>"
        , "<html>"
        , "  <head>"
        , "    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />"
        , "    <title>" ++ stripTags title ++ "</title>"
        , "    <link rel=\"stylesheet\" type=\"text/css\" href=\"anatomy.css\" />"
        , "    <link rel=\"stylesheet\" type=\"text/css\" href=\"highlight.css\" />"
        , "    <script src=\"jquery.js\" type=\"text/javascript\"></script>"
        , "    <script src=\"jquery.hotkeys.js\" type=\"text/javascript\"></script>"
        , "    <script src=\"tags.js\" type=\"text/javascript\"></script>"
        , "    <script src=\"main.js\" type=\"text/javascript\"></script>"
        , "  </head>"
        , "  <body class=\"" ++ classes ++ "\">"

        , if showTOC
            then unlines
                [ "    <div id=\"sidebar\">"
                , "      <form class=\"search\" action=\"javascript:void(0)\">"
                , "        <input type=\"text\" id=\"search\" />"
                , "      </form>"
                , "      <ul class=\"search-results\"></ul>"
                , "      <h4>On this page:</h4>"
                , toc
                , case parent of
                    Nothing -> ""
                    Just p -> "<h4>Up one level:</h4>" ++ p
                , "    </div>"
                ]
            else ""

        , "    <div id=\"content\">"
        , contentFor (sectionStyle s) title (autoFlow body)
        , "    </div>"
        , "  </body>"
        , "</html>"
        ]

    return (particle "done")

contentFor :: Style -> String -> String -> String
contentFor s t b
    | styleMatch Annotated s =
        concat
            [ "<table class=\"annotated-source\">"
            , "<thead><tr><th class=\"prose\"><h1>" ++ t ++ "</h1></th><th class=\"code\"></th></tr></thead>"
            , b
            , "</table>"
            ]
    | otherwise =
        concat
            [ "<h1>" ++ t ++ "</h1>"
            , b
            ]

getAObject :: AVM Value
getAObject = do
    s <- get

    lift $ defineOn (sectionA s)
        (Slot (single "state" PThis) (Haskell (toDyn s)))

    return (sectionA s)

searchTags :: Section -> AVM [(String, String, String)]
searchTags s = do
    t <- build (titleText (sectionTitle s))
    u <- sectionURL s

    tag <-
        case titleTag (sectionTitle s) of
            Nothing -> return []
            Just s -> do
                x <- buildForString' s
                if null x
                    then return []
                    else return [(stripTags x, x, u)]

    bs <- forM (sectionBindings s) $ \b -> do
        u <- findBinding b s
        s <- getAObject
        String s <- lift . dispatch $
            keyword'
                ["highlight"]
                [s, string $ bindingName b]
                [option "autolink" (Boolean False)]

        return (bindingName b, fromText s, fromJust u)

    subts <- mapM searchTags (subSections s)
    return ([(stripTags t, t, u)] ++ tag ++ bs ++ concat subts)

findSection :: String -> Section -> AVM (Maybe Section)
findSection n s = do
    tag <-
        case titleTag (sectionTitle s) of
            Nothing -> return Nothing
            Just t -> fmap Just $ buildForString' t
    title <- build (titleText (sectionTitle s))
    if n == title || Just n == tag
        then return (Just s)
        else do

    kids <- findFirstSection n (subSections s)
    case (kids, sectionParent s) of
        (Just k, _) -> return (Just k)
        (Nothing, Nothing) -> return Nothing
        (Nothing, Just pr) -> do
            p <- liftIO (readIORef pr)
            findSection n p

findSectionDownward :: String -> Section -> AVM (Maybe Section)
findSectionDownward n s = do
    tag <-
        case titleTag (sectionTitle s) of
            Nothing -> return Nothing
            Just t -> fmap Just $ buildForString' t

    title <- build (titleText (sectionTitle s))
    if n == title || Just n == tag
        then return (Just s)
        else findFirstSection n (subSections s)

findFirstSection :: String -> [Section] -> AVM (Maybe Section)
findFirstSection _ [] = return Nothing
findFirstSection k (s:ss) = do
    f <- findSectionDownward k s
    maybe (findFirstSection k ss) (return . Just) f

findBinding :: BindingKey -> Section -> AVM (Maybe String)
findBinding k s =
    if k `elem` sectionBindings s
        then fmap Just (bindingURL s k)
        else do

    kids <- findFirstBinding k (subSections s)
    case (kids, sectionParent s) of
        (Just b, _) -> return (Just b)
        (Nothing, Nothing) -> return Nothing
        (Nothing, Just pr) -> do
            p <- liftIO (readIORef pr)
            findBinding k p

findBindingDownward :: BindingKey -> Section -> AVM (Maybe String)
findBindingDownward k s =
    if k `elem` sectionBindings s
        then fmap Just (bindingURL s k)
        else findFirstBinding k (subSections s)

findFirstBinding :: BindingKey -> [Section] -> AVM (Maybe String)
findFirstBinding _ [] = return Nothing
findFirstBinding k (s:ss) = do
    f <- findBindingDownward k s
    maybe (findFirstBinding k ss) (return . Just) f

bindingURL :: Section -> BindingKey -> AVM String
bindingURL s k =
    sectionURL s
        >>= \u -> return $ trimFragment u ++ "#" ++ bindingID k

sectionURL :: Section -> AVM String
sectionURL s@(Section { sectionParent = Nothing }) =
    pageURL s
sectionURL s@(Section { sectionParent = Just sr }) = do
    p <- liftIO (readIORef sr)
    if styleMatch TOC (sectionStyle p)
        then pageURL s
        else do
            st <- buildForString' (tagOrTitle s)
            purl <- sectionURL p
            return (trimFragment purl ++ "#section_" ++ sanitize st)

pageURL :: Section -> AVM String
pageURL s = fmap ((<.> "html") . sanitize) (buildForString' (tagOrTitle s))

tagOrTitle :: Section -> Segment
tagOrTitle (Section { sectionTitle = Title { titleTag = Just t } }) =
    t
tagOrTitle s = titleText (sectionTitle s)

sanitize :: String -> String
sanitize "" = ""
sanitize (' ':ss) = '_' : sanitize ss
sanitize ('.':ss) = ".." ++ sanitize ss
sanitize ('<':ss) = sanitize (tail $ dropWhile (/= '>') ss)
sanitize (s:ss)
    | isUpper s = '.' : s : sanitize ss
    | isAlphaNum s || s `elem` "_-" = s : sanitize ss
    | otherwise = '_' : sanitize ss

buildForString' :: Segment -> AVM String
buildForString' (Atomo e) = lift (liftM (fromText . fromString) (macroExpand e >>= eval))
buildForString' x = build x

trimFragment :: String -> String
trimFragment = takeWhile (/= '#')

stripTags :: String -> String
stripTags = innerText . parseTags
