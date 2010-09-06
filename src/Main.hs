module Main where

import System.Environment (getArgs)

import Anatomy.Builder


main :: IO ()
main = do
    args <- getArgs

    case args of
        [fn, "-o", out] -> buildFile fn out
        [fn] -> buildFile fn "."
        _ -> putStrLn . unlines $
            [ "usage:"
            , "anatomy FILENAME\t\tbuild FILENAME, outputting documents to ."
            , "anatomy FILENAME -o DIRNAME\tbuild FILENAME, outputting documents to DIRNAME"
            ]
