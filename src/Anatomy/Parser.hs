module Anatomy.Parser (parse, parseFile, parseDefinition, continuedParse, continuedParseFile) where

import "monads-fd" Control.Monad.Error
import "monads-fd" Control.Monad.State
import Data.Char (isSpace)
import Data.Hashable (hash)
import Data.List (intercalate)
import Text.Parsec hiding (parse)
import Atomo.Parser.Base (Parser)
import qualified Atomo.Types as AT
import qualified Atomo.Parser as AP
import qualified Atomo.Parser.Base as AB

import Anatomy.Debug
import Anatomy.Types


special :: Char
special = '#'

nested :: Parser [Segment]
nested = do
    block <- balancedBetween '{' '}'
    case runParser parser [] "<nested>" (cleanup block) of
        Left e -> fail ("nested: " ++ show e)
        Right ok -> return ok
  where
    cleanup s = intercalate "\n" . map (drop indentLevel) $ ls
      where
        ls = dropWhile null . lines $ s
        indentLevel = length . takeWhile isSpace $ head ls


chunk :: Parser Segment
chunk = do
    text <- anyToken `manyTill` (eof <|> lookAhead (char special >> return ()))
    dump ("got chunk", text)
    if null text
        then fail "empty chunk"
        else return (Chunk text)

keyword :: Parser Segment
keyword = fmap (debug "keyword") $ do
    char special
    dump "got special"
    ks <- many1 . try $ do
        name <- AB.anyIdent
        dump ("got identifier", name)
        char ':'
        val <- choice
            [ fmap Nested nested
            , try $ fmap The . choice $
                -- literal value
                [ unlexeme AP.pLiteral

                -- arbitrary expr
                , try $ between (char '(') (char ')') AP.pExpr
                ]

            -- operator reference
            , try $ fmap (\x -> The $ AT.Dispatch Nothing (AT.EKeyword (hash [x]) [x] [AT.ETop Nothing, AT.ETop Nothing])) $
                between (char '(') (char ')') (many1 (oneOf (':':AB.opLetters)))

            -- keyword reference
            , try $ fmap (\ks -> The $ AT.Dispatch Nothing (AT.EKeyword (hash ks) ks (replicate (length ks + 1) (AT.ETop Nothing)))) $
                between (char '(') (char ')') (many1 (AB.identifier >>= \n -> char ':' >> return n))

            -- single reference; trailing punctuation is ignored
            , do
                ident <- AB.anyIdent

                let punct = reverse . takeWhile (`elem` AB.opLetters) . reverse $ ident
                    sane = reverse . dropWhile (`elem` AB.opLetters) . reverse $ ident
                
                getInput >>= setInput . (punct ++)

                return . The . AT.Dispatch Nothing $ AT.ESingle (hash sane) sane (AT.ETop Nothing)
            ]
        return (name, val)

    let (ns, vs) = unzip ks

    return (KeywordDispatch ns vs)

single :: Parser Segment
single = fmap (debug "single") $ do
    char special
    name <- AB.identifier
    dump ("got single identifier", name)
    return (SingleDispatch name)

the :: Parser Segment
the = fmap (debug "the") $ do
    char special
    fmap The (between (char '(') (char ')') AP.pExpr)

parser :: Parser [Segment]
parser = many $ choice
    [ try keyword
    , try single
    , try the
    , chunk
    ]

parse :: String -> Either ParseError [Segment]
parse = runParser (do { r <- parser; eof; return r }) [] "<input>"

parseFile :: String -> IO (Either ParseError [Segment])
parseFile fn = fmap (runParser (do { r <- parser; eof; return r }) [] fn) (readFile fn)

defParser :: Parser Definition
defParser = do
    thumb <- try (fmap toDispatch (try AP.pSet <|> AP.pDefine)) <|> AP.pDispatch
    AB.whiteSpace
    cs <- many (try $ AB.symbol "|" >> AP.pDispatch >>= \d -> AB.whiteSpace >> return d)
    AB.whiteSpace
    ret <- AB.symbol ">" >> AP.pDispatch
    return Definition
        { defThumb = thumb
        , defContracts = cs
        , defReturn = ret
        }
  where
    toDispatch (AT.Define { AT.eLocation = l, AT.ePattern = p, AT.eExpr = e }) =
        AT.Dispatch l (AT.EKeyword (hash [":="]) [":="] [AT.Primitive l (AT.Pattern p), e])
    toDispatch (AT.Set { AT.eLocation = l, AT.ePattern = p, AT.eExpr = e }) =
        AT.Dispatch l (AT.EKeyword (hash ["="]) ["="] [AT.Primitive l (AT.Pattern p), e])

-- restore the whitespace that a lexeme parser nom'd up
unlexeme :: Parser a -> Parser a
unlexeme p = do
    before <- getInput
    r <- p
    after <- getInput
    backtrack before after
    return r
  where
    backtrack b a = do
        setInput (trailing ++ a)
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


continuedParser :: Parser a -> String -> String -> AT.VM a
continuedParser p i s = do
    ps <- gets AT.parserState
    case runParser (do { r <- p; s <- getState; return (s, r) }) ps s i of
        Left e -> throwError (AT.ParseError e)
        Right (ps', es) -> do
            modify $ \e -> e { AT.parserState = ps' }
            return es

-- | parse input i from source s, maintaining parser state between parses
continuedParse :: String -> String -> AT.VM [Segment]
continuedParse = continuedParser parser

continuedParseFile :: FilePath -> AT.VM [Segment]
continuedParseFile fn = liftIO (readFile fn) >>= flip continuedParse fn

parseDefinition :: String -> AT.VM Definition
parseDefinition = continuedParser
    (do { r <- defParser; eof; return r })
    "<inline>"

