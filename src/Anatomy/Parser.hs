module Anatomy.Parser (parseFile, parseDefinition) where

import Control.Monad.Error
import Data.Char (isSpace)
import Data.Hashable (hash)
import Data.List (intercalate)
import Text.Parsec hiding (parse)
import Atomo.Parser (continue)
import Atomo.Parser.Base (Parser)
import Atomo.Parser.Expand
import Atomo.Parser.Expr hiding (parser)
import qualified Atomo.Types as AT
import qualified Atomo.Parser.Base as AB

import Anatomy.Debug
import Anatomy.Types


special :: Char
special = '#'

nested :: Parser [Segment]
nested = do
    ps <- getState
    pos <- getPosition
    block <- balancedBetween '{' '}'
    res <- lift $ runParserT parser ps (show pos) (cleanup block)
    case res of
        Left e -> fail ("nested: " ++ show e)
        Right ok -> return ok
  where
    cleanup s = intercalate "\n" . map (drop indentLevel) $ ls
      where
        ls = dropWhile null . lines $ s
        indentLevel = length . takeWhile isSpace $ head ls


chunk :: Parser Segment
chunk = do
    c <- satisfy (/= special)
    cs <- anyToken `manyTill` (eof <|> lookAhead (char special >> return ()))
    dump ("got chunk", c:cs)
    return (Chunk (c:cs))

keyword :: Parser Segment
keyword = do
    char special
    ks <- many1 . try $ do
        name <- AB.anyIdent
        char ':'
        val <- choice
            [ fmap Nested nested
            , try $ fmap Atomo . choice $
                -- literal value
                [ unlexeme pLiteral

                -- arbitrary expr
                , try . between (char '(') (char ')') $
                    pExpr
                ]

            -- operator reference
            , try $ fmap (\x -> Atomo $ AT.Dispatch Nothing (AT.ekeyword [x] [])) $
                between (char '(') (char ')')
                    (many1 (satisfy AB.isOpLetter))

            -- keyword reference
            , try $ fmap (\ks -> Atomo $ AT.Dispatch Nothing (AT.ekeyword ks [])) $
                between (char '(') (char ')')
                    (many1 (AB.identifier >>= \n -> char ':' >> return n))

            -- single reference; trailing punctuation is ignored
            , do
                ident <- AB.anyIdent

                let punct = reverse . takeWhile AB.isOpLetter . reverse $ ident
                    sane = reverse . dropWhile AB.isOpLetter . reverse $ ident

                getInput >>= setInput . (punct ++)

                return . Atomo . AT.Dispatch Nothing $ AT.ESingle (hash sane) sane (AT.ETop Nothing)
            ]
        dump ("got value", val)
        return (name, val)

    let (ns, vs) = unzip ks

    return (KeywordDispatch ns vs)

single :: Parser Segment
single = fmap (debug "single") $ do
    char special
    name <- AB.identifier
    notFollowedBy (char ':')
    dump ("got single identifier", name)
    return (SingleDispatch name)

atomo :: Parser Segment
atomo = fmap (debug "atomo") $ do
    char special
    fmap Atomo (between (char '(') (char ')') pExpr >>= macroExpand)

parser :: Parser [Segment]
parser = do
    ss <- many $ choice
        [ try keyword
        , try single
        , try atomo
        , chunk
        ]
    eof
    return ss

parseFile :: String -> AT.VM [Segment]
parseFile fn = liftIO (readFile fn) >>= continue parser fn

defParser :: Parser Definition
defParser = do
    thumb <- pDispatch

    AB.whiteSpace

    cs <- many . try $ do
        AB.symbol "|"
        d <- pDispatch
        AB.whiteSpace
        return d

    AB.whiteSpace

    ret <- AB.symbol ">" >> pDispatch

    return Definition
        { defThumb = thumb
        , defContracts = cs
        , defReturn = ret
        }

-- restore the whitespace that a lexeme parser nom'd up
unlexeme :: Parser a -> Parser a
unlexeme p = do
    before <- getInput
    r <- p
    after <- getInput
    backtrack before after
    return r
  where
    backtrack b a = setInput (trailing ++ a)
      where
        trailing
            = reverse
            . takeWhile isSpace
            . reverse
            . take (length b - length a)
            $ b

-- grab text between characters, balanced
balancedBetween :: Char -> Char -> Parser String
balancedBetween o c = try $ do
    char o
    raw <- many . choice $
        [ many1 $ noneOf [o, c]
        , do
            res <- balancedBetween o c
            return $ o : res ++ [c]
        ]
    char c
    return $ concat raw

parseDefinition :: String -> AT.VM Definition
parseDefinition = continue
    (do { r <- defParser; eof; return r })
    "<definition>"

