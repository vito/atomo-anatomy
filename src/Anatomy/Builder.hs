{-# LANGUAGE QuasiQuotes #-}
module Anatomy.Builder where

import Control.Monad.Error
import Control.Monad.State hiding (State)
import Data.Char
import Data.Dynamic
import Data.Hashable (hash)
import Data.IORef
import System.FilePath
import Text.HTML.TagSoup
import qualified Data.IntMap as IM
import qualified Data.Vector as V

import Anatomy.AutoFlow
import Anatomy.Parser
import Anatomy.Types

import Atomo.Environment
import Atomo.Haskell
import Atomo.Method
import Atomo.Pretty
import Atomo.Types

import Paths_anatomy


-- scan through everything and build up the initial state for generation
scan :: Int -> Int -> [Segment] -> VM Section
scan d n ss' = do
    sec <- liftIO . newSection $ \s -> s { sectionDepth = d, sectionNumber = n }
    st <- scan' sec ss'
    liftIO (writeIORef (sectionID st) st)
    return st
  where
    scan' acc [] = return acc
    scan' acc (KeywordDispatch ["title"] [s]:ss) =
        scan' (acc { sectionTitle = Title s Nothing Nothing }) ss
    scan' acc (KeywordDispatch ["title", "tag"] [s, t]:ss) =
        scan' (acc { sectionTitle = Title s (Just t) Nothing }) ss
    scan' acc (KeywordDispatch ["title", "tag", "version"] [s, t, v]:ss) =
        scan' (acc { sectionTitle = Title s (Just t) (Just v) }) ss
    scan' acc (KeywordDispatch ["include-section"] [sfn]:ss) = do
        cfn <- buildForString sfn
        p <- liftIO (parseFile cfn)
        case p of
            Right ast -> do
                sec <- fmap (\s -> s { sectionParent = Just (sectionID acc) }) $
                    scan (d + 1) (length (subSections acc) + 1) ast

                liftIO (writeIORef (sectionID sec) sec)

                scan' (acc
                    { sectionBody = sectionBody acc ++
                        [SectionReference (length (subSections acc))]
                    , subSections = subSections acc ++ [sec]
                    }) ss
            Left e -> error $ "error including section: " ++ show e
    scan' acc (KeywordDispatch ["section"] [st]:ss) = do
        sec <- fmap (\s -> s
            { sectionTitle = Title st Nothing Nothing
            , sectionParent = Just (sectionID acc)
            }) $ scan (d + 1) (length (subSections acc) + 1) sb

        liftIO (writeIORef (sectionID sec) sec)

        scan' (acc
            { sectionBody = sectionBody acc ++
                [SectionReference (length (subSections acc))]
            , subSections = subSections acc ++ [sec]
            }) rest
      where
        (sb, rest) = span (\s ->
            case s of
                KeywordDispatch ["section"] _ -> False
                _ -> True) ss
    scan' acc (KeywordDispatch ["define"] [sb]:ss) = do
        body <- buildForString sb
        case parseDefinition body of
            Right def ->
                scan' (acc
                    { sectionBody = sectionBody acc ++ [InlineDefinition def Nothing]
                    , sectionBindings = defKey def : sectionBindings acc
                    }) ss
            Left e ->
                error $ "error parsing definition: " ++ show e ++ "\ndefinition:\n" ++ body
    scan' acc (KeywordDispatch ["define", "body"] [sd, sb]:ss) = do
        body <- buildForString sd
        case parseDefinition body of
            Right def ->
                scan' (acc
                    { sectionBody = sectionBody acc ++ [InlineDefinition def (Just sb)]
                    , sectionBindings = defKey def : sectionBindings acc
                    }) ss
            Left e ->
                error $ "error parsing definition: " ++ show e ++ "\ndefinition:\n" ++ body
    scan' acc (SingleDispatch "table-of-contents":ss) =
        scan' (acc
            { sectionStyle = TOC
            , sectionBody = sectionBody acc ++ [TableOfContents]
            }) ss
    scan' acc (s:ss) =
        scan' (acc { sectionBody = sectionBody acc ++ [s] }) ss


buildForString :: Segment -> VM String
buildForString (The e) = eval e >>= valToString
buildForString (Chunk s) = return s -- TODO: escaping
buildForString (Nested ns) = fmap concat (mapM buildForString ns)
buildForString x = error $ "cannot be built into a string: " ++ show x

build :: Segment -> AVM String
build (Chunk s) = return s
build (KeywordDispatch ["section"] [n]) = do
    sn <- build n
    return $ "<h4>" ++ sn ++ "</h4>"
build (KeywordDispatch ns ss) = do
    vs <- forM ss $ \s ->
        case s of
            The e -> return (Expression e)
            _ -> build s >>= string

    s <- getAProto
    res <- lift (dispatch (Keyword (hash ns) ns (s:vs)))
    lift (valToString res)
build (SingleDispatch n) = do
    s <- getAProto
    res <- lift (dispatch (Single (hash n) n s))
    lift (valToString res)
build (The e) = lift (eval e >>= return . show . pretty)
build (Nested ss) = fmap concat $ mapM build ss
build (SectionReference n) = do
    style <- gets sectionStyle
    if style == TOC
        then return ""
        else do

    sec <- gets ((!! n) . subSections)
    flip runAVM sec $ do
        title <- gets (titleText . sectionTitle)
        depth <- do
            myd <- gets sectionDepth
            mp <- gets sectionParent
            case mp of
                Nothing -> return myd
                Just p -> do
                    pd <- fmap sectionDepth $ liftIO (readIORef p)
                    return (myd - pd)

        t <- build title
        b <- mapM build (sectionBody sec)
        let header = "h" ++ show (depth + 1)
        return . unlines $
            [ "<div class=\"section\">"
            , "  " ++ concat
                [ "<" ++ header ++ " class=\"section-header\" id=\"section_" ++ sanitize t ++ "\">"
                , t
                , "</" ++ header ++ ">"
                ]
            , "  " ++ concat b
            , "</div>"
            ]
build TableOfContents =
    get >>= buildTOC >>= return . printTOC
build FullTableOfContents =
    get >>= buildTOC >>= return . printFullTOC
build (InlineDefinition d b) = do
    a <- getAProto
    thumb <- lift $ dispatch (Keyword (hash ["pretty"]) ["pretty"] [a, Expression (defThumb d)]) >>= valToString
    pr <- lift $ dispatch (Keyword (hash ["pretty"]) ["pretty"] [a, Expression (defReturn d)]) >>= valToString
    pcs <- lift $ mapM (\c -> dispatch (Keyword (hash ["pretty"]) ["pretty"] [a, Expression c]) >>= valToString) (defContracts d)
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
        Branch _ _ ss -> "<ol class=\"toc\">" ++ concatMap printTOC' ss ++ "</ol>"
        _ -> "<ol class=\"toc\">" ++ printTOC' t ++ "</ol>"

printFullTOC :: TOCTree -> String
printFullTOC t = "<ol class=\"toc\">" ++ printTOC' t ++ "</ol>"

printTOC' :: TOCTree -> String
printTOC' (Node u n) = "<li><a href=\"" ++ u ++ "\">" ++ n ++ "</a></li>"
printTOC' (Branch u n ss) = "<li><a href=\"" ++ u ++ "\">" ++ n ++ "</a><ol>" ++ concatMap printTOC' ss ++ "</ol></li>"

buildFile :: FilePath -> FilePath -> IO ()
buildFile fn o = do
    css <- getDataFileName "lib/anatomy.css" >>= readFile
    writeFile (o </> "anatomy.css") css

    p <- parseFile fn
    case p of
        Right ast -> do
            sec <- newSection id
            estart <- runA (lift $ scan 0 1 ast) sec

            flip (either (print . pretty)) estart $ \start -> do
                res <- runA (buildDocument o) start
                case res of
                    Left e -> print . pretty $ e
                    Right _ -> return ()
        Left e -> print e

buildDocument :: FilePath -> AVM ()
buildDocument o = do
    s <- get
    if sectionStyle s == TOC
        then mapM_ (runAVM (buildDocument o)) (subSections s)
        else return ()

    toc <- build TableOfContents
    title <- build . titleText . sectionTitle $ s
    body <- fmap concat $ mapM build (sectionBody s)

    parent <-
        case sectionParent s of
            Nothing -> return Nothing
            Just p ->
                liftIO (readIORef p)
                    >>= runAVM (build FullTableOfContents)
                    >>= return . Just

    fn <- sectionURL s
    liftIO . writeFile (o </> fn) $
        format toc title body parent
  where
    format toc t b mp = unlines
        [ "<!DOCTYPE html>"
        , "<html>"
        , "  <head>"
        , "    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />"
        , "    <title>" ++ stripTags t ++ "</title>"
        , "    <link rel=\"stylesheet\" type=\"text/css\" href=\"anatomy.css\" />"
        , "  </head>"
        , "  <body>"
        , "    <div id=\"sidebar\">"
        , "       <h4>On this page:</h4>"
        , toc
        , case mp of
              Nothing -> ""
              Just p -> concat
                [ "<h4>Up one level:</h4>"
                , p
                ]
        , "    </div>"
        , "    <div id=\"content\">"
        , "      <h1>" ++ t ++ "</h1>"
        , autoFlow b
        , "    </div>"
        , "  </body>"
        , "</html>"
        ]

getAProto :: AVM Value
getAProto = do
    s <- get
    a <- lift (here "A")
    lift . newObject $ \o -> o
        { oDelegates = oDelegates o ++ [a]
        , oMethods = 
            ( addMethod (Slot (PSingle (hash "state") "state" PSelf) (Haskell (toDyn s))) IM.empty
            , IM.empty
            )
        }
    
runAVM :: AVM a -> Section -> AVM a
runAVM a s = lift (evalStateT a s)

runAVM' :: AVM a -> Section -> VM a
runAVM' = evalStateT

runA :: AVM a -> Section -> IO (Either AtomoError a)
runA a s = evalStateT (runErrorT (initEnv >> initA >> evalStateT a s)) startEnv

newSection :: (Section -> Section) -> IO Section
newSection f = do
    r <- newIORef undefined

    writeIORef r . f $ Section
        { sectionID = r
        , sectionTitle = Title (Chunk "Untitled") Nothing Nothing
        , sectionStyle = None
        , sectionBody = []
        , sectionBindings = []
        , sectionParent = Nothing
        , subSections = []
        , sectionDepth = 0
        , sectionNumber = 1
        }

    readIORef r

initA :: VM ()
initA = do
    liftIO (getDataFileName "lib/core.atomo") >>= loadFile

    [$p|(a: A) url-for: (e: Expression)|] =: do
        Expression ae <- here "e" >>= findValue isExpression
        Haskell a <- eval [$e|a state|]

        let st = fromDyn a (error "hotlink A is invalid") :: Section

        find <-
            case ae of
                Dispatch { eMessage = ESingle { emName = n } } -> do
                    runAVM' (findBinding (SingleKey n) st) st
                Dispatch { eMessage = EKeyword { emNames = ns } } -> do
                    runAVM' (findBinding (KeywordKey ns) st) st
                _ -> error $ "no @url-for: for " ++ show (pretty ae)

        case find of
            Nothing -> return (particle "none")
            Just u -> do
                url <- string u
                return (keyParticle ["ok"] [Nothing, Just url])

    [$p|(a: A) reference: (s: String)|] =: do
        n <- fmap V.toList $ getList [$e|s|]
        Haskell a <- eval [$e|a state|]

        let st = fromDyn a (error "hotlink A is invalid") :: Section

        flip runAVM' st $ do
            ms <- findSection (map (\(Char c) -> c) n) st
            case ms of
                Nothing -> list n
                Just s -> do
                    url <- sectionURL s
                    name <- build (titleText (sectionTitle s))
                    string $ "<a href=\"" ++ url ++ "\">" ++ name ++ "</a>"

findSection :: String -> Section -> AVM (Maybe Section)
findSection n s = do
    tag <-
        case titleTag (sectionTitle s) of
            Nothing -> return Nothing
            Just s -> fmap Just $ buildForString' s
    title <- build (titleText (sectionTitle s))
    if n == title || Just n == tag
        then return (Just s)
        else do

    kids <- findFirstSection n (subSections s)
    case (kids, sectionParent s) of
        (Just s, _) -> return (Just s)
        (Nothing, Nothing) -> return Nothing
        (Nothing, Just pr) -> do
            p <- liftIO (readIORef pr)
            findSection n p

findSectionDownward :: String -> Section -> AVM (Maybe Section)
findSectionDownward n s = do
    tag <-
        case titleTag (sectionTitle s) of
            Nothing -> return Nothing
            Just s -> fmap Just $ buildForString' s

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
        (Just s, _) -> return (Just s)
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
sectionURL s =
    case sectionParent s of
        Nothing -> pageURL
        Just sr -> do
            p <- liftIO (readIORef sr)
            if sectionStyle p == TOC
                then pageURL
                else do
                    st <- build (titleText (sectionTitle s))
                    purl <- sectionURL p
                    return (trimFragment purl ++ "#section_" ++ sanitize st)
  where
    pageURL = fmap ((<.> "html") . sanitize) $
        case sectionTitle s of
            Title { titleTag = Just tag } ->
                buildForString' tag
            Title { titleText = text } ->
                buildForString' text

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
buildForString' (The e) = lift (eval e >>= valToString)
buildForString' x = build x

valToString :: Value -> VM String
valToString (List v) =
    fmap (map (\(Char c) -> c) . V.toList) (liftIO (readIORef v))
valToString x = error $ "not a string: " ++ show x

trimFragment :: String -> String
trimFragment = takeWhile (/= '#')

stripTags :: String -> String
stripTags = innerText . parseTags
