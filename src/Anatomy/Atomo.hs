{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Anatomy.Atomo where

import Data.Dynamic
import Data.Text.Encoding
import Prelude hiding (div, span)
import System.Directory
import System.FilePath
import Text.Blaze.Html5 hiding (p, string)
import Text.Blaze.Html5.Attributes hiding (name, span)
import Text.Blaze.Renderer.String
import Text.Highlighter.Formatters.Html
import Text.Highlighter.Lexers.Atomo (lexer)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Text.Highlighter as HL

import Atomo
import Atomo.Load

import Anatomy.Builder
import Anatomy.Parser
import Anatomy.Scanner
import Anatomy.Types

import Paths_anatomy


load :: VM ()
load = do
    ([$p|A|] =::) =<< eval [$e|Object clone|]

    liftIO (getDataFileName "lib/core.atomo") >>= loadFile

    [$p|(a: A) new: (fn: String)|] =: do
        fn <- getString [$e|fn|]

        path <- fmap takeDirectory . liftIO $ canonicalizePath fn

        liftIO (putStrLn ("path: " ++ path))
        ast <- parseFile fn
        sec <- scan 0 1 path ast
        [$p|a state|] =:: Haskell (toDyn sec)
        here "a"

    [$p|(a: A) url-for: (e: Expression)|] =: do
        Expression ae <- here "e" >>= findExpression
        Haskell ds <- eval [$e|a state|]

        let st = fromDyn ds (error "hotlink A is invalid") :: Section

        find <-
            case ae of
                EDispatch { eMessage = Single { mName = n } } ->
                    runAVM' (findBinding (SingleKey n) st) st
                EDispatch { eMessage = Keyword { mNames = ns } } ->
                    runAVM' (findBinding (KeywordKey ns) st) st

                EParticle { eParticle = Single { mName = n } } ->
                    runAVM' (findBinding (SingleKey n) st) st
                EParticle { eParticle = Keyword { mNames = ns } } ->
                    runAVM' (findBinding (KeywordKey ns) st) st

                EPrimitive { eValue = Particle (Single { mName = n }) } ->
                    runAVM' (findBinding (SingleKey n) st) st
                EPrimitive { eValue = Particle (Keyword { mNames = ns }) } ->
                    runAVM' (findBinding (KeywordKey ns) st) st

                _ -> raise ["no-url-for"] [Expression ae]

        case find of
            Nothing -> return (particle "none")
            Just u ->
                return (keyParticle ["ok"] [Nothing, Just (string u)])

    [$p|(a: A) reference: (s: String)|] =: do
        n <- getString [$e|s|]
        Haskell ds <- eval [$e|a state|]

        let st = fromDyn ds (error "hotlink A is invalid") :: Section

        flip runAVM' st $ do
            ms <- findSection n st
            case ms of
                Nothing -> return (string n)
                Just s -> do
                    url <- sectionURL s
                    name <- buildForString' (titleText (sectionTitle s))
                    return (string $ "<a href=\"" ++ url ++ "\">" ++ name ++ "</a>")

    [$p|(a: A) atomo: (s: String)|] =: do
        s <- getText [$e|s|]
        Haskell ds <- eval [$e|a state|]

        let st = fromDyn ds (error "hotlink A is invalid") :: Section

        case HL.runLexer lexer (encodeUtf8 s) of
            Left err ->
                error ("lexing of Atomo source failed: " ++ show (s, err))
            Right ts ->
                liftM (string . renderHtml . (div ! class_ (stringValue "highlight")) . pre) (runAVM' (autoLink ts) st)

    [$p|(a: A) highlight: (s: String) &auto-link|] =: do
        s <- getText [$e|s|]
        Haskell ds <- eval [$e|a state|]
        Boolean auto <- here "auto-link" >>= findBoolean

        let st = fromDyn ds (error "hotlink A is invalid") :: Section

        case HL.runLexer lexer (encodeUtf8 s) of
            Left err ->
                error ("@highlight: - lexing failed: " ++ show (s, err))
            Right ts | auto ->
                liftM (string . renderHtml) (runAVM' (autoLink ts) st)
            Right ts ->
                return (string (renderHtml (formatInline ts)))

    [$p|(a: A) highlight: (s: String) as: (lang: String)|] =: do
        s <- getText [$e|s|]
        l <- getString [$e|lang|] >>= \n ->
            case HL.lexerFromFilename ("." ++ n) of
                Nothing -> raise ["unknown-lexer"] [string n]
                Just l -> return l

        case HL.runLexer l (encodeUtf8 s) of
            Left err ->
                error ("@highlight:as: - lexing failed: " ++ show (s, err))
            Right ts ->
                return (string (renderHtml (formatInline ts)))



-- Format lexed source, auto-linking dispatches to their definitions.
autoLink :: [HL.Token] -> AVM Html
autoLink = autoLink' ([], "")
  where
    autoLink' _ [] = return (return ())
    autoLink' cks (HL.Token HL.Name s:ts) = do
        st <- get
        mu <- findBinding (SingleKey (fromBS s)) st
        case mu of
            Nothing -> do
                rest <- autoLink' cks ts
                return $ (span ! class_ (stringValue "n") $ bs s) >> rest
            Just u -> do
                rest <- autoLink' cks ts
                return $ (span ! class_ (stringValue "n") $ a ! href (stringValue u) $ bs s) >> rest
    autoLink' cks (HL.Token HL.Operator s:ts) = do
        st <- get
        mu <- findBinding (KeywordKey [fromBS s]) st
        case mu of
            Nothing -> do
                rest <- autoLink' cks ts
                return $ (span ! class_ (stringValue "o") $ bs s) >> rest
            Just u -> do
                rest <- autoLink' cks ts
                return $ (span ! class_ (stringValue "o") $ a ! href (stringValue u) $ bs s) >> rest
    autoLink' ([], _) (HL.Token (HL.Name HL.:. HL.Function) s:ts) = do
        st <- get
        let full = init (fromBS s) : restOf ts
        mu <- findBinding (KeywordKey full) st
        case mu of
            Nothing -> do
                rest <- autoLink' (tail full, "") ts
                return $ (span ! class_ "nf" $ bs s) >> rest
            Just u -> do
                rest <- autoLink' (tail full, u) ts
                return $ (span ! class_ "nf" $ a ! href (stringValue u) $ bs s) >> rest
    autoLink' (cks, u) (HL.Token (HL.Name HL.:. HL.Function) s:ts) = do
        rest <- autoLink' (tail cks, u) ts
        if not (null u)
            then return $ (span ! class_ "nf" $ a ! href (stringValue u) $ bs s) >> rest
            else return $ (span ! class_ "nf" $ bs s) >> rest
    autoLink' cks (HL.Token t s:ts) = do
        rest <- autoLink' cks ts
        return $ (span ! class_ (stringValue $ HL.shortName t) $ bs s) >> rest

-- Retrieve the rest of a keyword message name from a list of tokens.
restOf :: [HL.Token] -> [String]
restOf [] = []
restOf (HL.Token _ "(":ts) =
    restOf (drop 1 $ dropWhile ((/= "(") . HL.tText) $ ts)
restOf (HL.Token _ "{":ts) =
    restOf (drop 1 $ dropWhile ((/= "}") . HL.tText) $ ts)
restOf (HL.Token _ "[":ts) =
    restOf (drop 1 $ dropWhile ((/= "]") . HL.tText) $ ts)
restOf (HL.Token _ ")":_) = []
restOf (HL.Token _ "}":_) = []
restOf (HL.Token _ "]":_) = []
restOf (HL.Token (HL.Name HL.:. HL.Function) n:ts) =
    init (fromBS n) : restOf ts
restOf (HL.Token HL.Text x:_)
    | toEnum (fromEnum '\n') `BS.elem` x = []
restOf (_:ts) = restOf ts

bs :: BS.ByteString -> Html
bs = text . decodeUtf8

fromBS :: BS.ByteString -> String
fromBS = T.unpack . decodeUtf8
