module Anatomy.Scanner where

import Control.Monad.State
import Data.IORef
import System.Directory
import System.FilePath

import Anatomy.Parser
import Anatomy.Types

import Atomo.Environment
import Atomo.Helpers
import Atomo.Types


-- scan through everything and build up the initial state for generation
scan :: Int -> Int -> FilePath -> [Segment] -> VM Section
scan depth num path ss' = do
    a <- here "A" >>= dispatch . single "clone"
    env <- here "Lobby" >>= dispatch . single "clone"
    defineOn a (Slot (psingle "environment" PThis) env)

    sec <- liftIO . newSection $ \s -> s
        { sectionDepth = depth
        , sectionNumber = num
        , sectionPath = path
        , sectionA = a
        }

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
        fn <- findFile [sectionPath acc, ""] cfn
        liftIO (putStrLn ("including section: " ++ fn))

        ast <- parseFile fn
        sec <- fmap (\s -> s { sectionParent = Just (sectionID acc) }) $
            scan (depth + 1) (length (subSections acc) + 1) (takeDirectory fn) ast

        liftIO (writeIORef (sectionID sec) sec)

        scan' (acc
            { sectionBody = sectionBody acc ++
                [SectionReference (length (subSections acc))]
            , subSections = subSections acc ++ [sec]
            }) ss
      where
        findFile [] fn = throwError (FileNotFound fn)
        findFile (p:ps) fn = do
            check <- liftIO . doesFileExist $ p </> fn

            if check
                then liftIO (canonicalizePath (p </> fn))
                else findFile ps fn

    scan' acc (KeywordDispatch ["section", "tag"] [s, t]:ss) = do
        subsection acc s (Just t) ss
    scan' acc (KeywordDispatch ["section"] [s]:ss) =
        subsection acc s Nothing ss
    scan' acc (KeywordDispatch ["define"] [sb]:ss) = do
        body <- buildForString sb

        def <- parseDefinition body
        scan' (acc
            { sectionBody = sectionBody acc ++ [InlineDefinition def Nothing]
            , sectionBindings = defKey def : sectionBindings acc
            }) ss
    scan' acc (KeywordDispatch ["define", "body"] [sd, sb]:ss) = do
        body <- buildForString sd
        def <- parseDefinition body
        scan' (acc
            { sectionBody = sectionBody acc ++ [InlineDefinition def (Just sb)]
            , sectionBindings = defKey def : sectionBindings acc
            }) ss
    scan' acc (SingleDispatch "table-of-contents":ss) = do
        liftIO (putStrLn "table of contents")

        scan' (acc
            { sectionStyle = TOC
            , sectionBody = sectionBody acc ++ [TableOfContents]
            }) ss
    scan' acc (s:ss) =
        scan' (acc { sectionBody = sectionBody acc ++ [s] }) ss

    subsection acc s t ss = do
        liftIO (putStrLn ("subsection: " ++ show s))

        sec <- fmap (\sec -> sec
            { sectionTitle = Title s t Nothing
            , sectionParent = Just (sectionID acc)
            }) $ scan (depth + 1) (length (subSections acc) + 1) "" sb

        liftIO (writeIORef (sectionID sec) sec)

        scan' (acc
            { sectionBody = sectionBody acc ++
                [SectionReference (length (subSections acc))]
            , subSections = subSections acc ++ [sec]
            }) rest
      where
        (sb, rest) = span (\sec ->
            case sec of
                KeywordDispatch ["section"] _ -> False
                KeywordDispatch ["section", "tag"] _ -> False
                _ -> True) ss


buildForString :: Segment -> VM String
buildForString (Atomo e) = liftM (fromText . fromString) (eval e)
buildForString (Chunk s) = return s -- TODO: escaping
buildForString (Nested ns) = fmap concat (mapM buildForString ns)
buildForString x = error $ "cannot be built into a string: " ++ show x
