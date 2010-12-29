{-# LANGUAGE QuasiQuotes #-}
module Anatomy.Atomo where

import Data.Dynamic
import System.Directory
import System.FilePath

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
        Haskell a <- eval [$e|a state|]

        let st = fromDyn a (error "hotlink A is invalid") :: Section

        find <-
            case ae of
                Dispatch { eMessage = ESingle { emName = n } } ->
                    runAVM' (findBinding (SingleKey n) st) st
                Dispatch { eMessage = EKeyword { emNames = ns } } ->
                    runAVM' (findBinding (KeywordKey ns) st) st

                EParticle { eParticle = EPMSingle n } ->
                    runAVM' (findBinding (SingleKey n) st) st
                EParticle { eParticle = EPMKeyword ns _ } ->
                    runAVM' (findBinding (KeywordKey ns) st) st

                Primitive { eValue = Particle (PMSingle n) } ->
                    runAVM' (findBinding (SingleKey n) st) st
                Primitive { eValue = Particle (PMKeyword ns _) } ->
                    runAVM' (findBinding (KeywordKey ns) st) st

                _ -> raise ["no-url-for"] [Expression ae]

        case find of
            Nothing -> return (particle "none")
            Just u ->
                return (keyParticle ["ok"] [Nothing, Just (string u)])

    [$p|(a: A) reference: (s: String)|] =: do
        n <- getString [$e|s|]
        Haskell a <- eval [$e|a state|]

        let st = fromDyn a (error "hotlink A is invalid") :: Section

        flip runAVM' st $ do
            ms <- findSection n st
            case ms of
                Nothing -> return (string n)
                Just s -> do
                    url <- sectionURL s
                    name <- buildForString' (titleText (sectionTitle s))
                    return (string $ "<a href=\"" ++ url ++ "\">" ++ name ++ "</a>")
