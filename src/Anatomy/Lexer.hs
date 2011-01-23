module Anatomy.Lexer where

import Control.Monad.Error
import Data.Char (isSpace)
import Data.List (intercalate)
import Text.Parsec hiding (parse)
import Atomo.Lexer.Base (Lexer, TaggedToken(..), Token(..))
import qualified Atomo.Lexer as AL
import qualified Atomo.Lexer.Base as AB

import Anatomy.Debug
import Anatomy.Types


special :: Char
special = '#'

nested :: Lexer [TaggedAToken]
nested = do
    ps <- getState
    pos <- getPosition
    block <- balancedBetween '{' '}'
    case runParser lexer ps (show pos) (cleanup block) of
        Left e -> fail ("nested: " ++ show e)
        Right ok -> return ok

nestedAtomo :: Lexer [TaggedToken]
nestedAtomo = do
    pos <- getPosition
    balancedBetween '(' ')' >>= subAtomo pos

subAtomo :: SourcePos -> String -> Lexer [TaggedToken]
subAtomo p s =
    case runParser AL.lexer (AB.LexerState []) (show p) s of
        Left e -> fail ("nested atomo: " ++ show e)
        Right ok -> return ok

cleanup :: String -> String
cleanup s = intercalate "\n" . map (drop indentLevel) $ ls
    where
    ls = dropWhile null . lines $ s
    indentLevel = length . takeWhile isSpace $ head ls


chunk :: Lexer AToken
chunk = do
    c <- sub
    cs <- manyTill sub (eof <|> lookAhead (char special >> return ()))
    dump ("got chunk", (c:cs))
    return (ATokChunk (c:cs))
  where
    sub = choice
        [ try $ do
            char '\\'
            char special
        , satisfy (/= special)
        ]

keyword :: Lexer AToken
keyword = do
    char special
    ks <- many1 . try $ do
        name <- AB.anyIdent
        char ':'
        val <- tagged $ choice
            [ fmap ATokNested nested
            , try $ fmap ATokAtomo . choice $
                -- some atomo code between parens
                [ nestedAtomo

                -- a list
                , do
                    p <- getPosition
                    c <- balancedBetween '[' ']'
                    subAtomo p ("[" ++ c ++ "]")

                -- a particle
                , choice
                    [ try $ do
                        p <- AB.tagged AL.lParticle
                        return [p]
                    , try $ do
                        p <- AB.tagged $ liftM TokPunctuation (char '@')
                        n <- AB.tagged $ liftM TokIdentifier AB.ident
                        return [p, n]
                    , do
                        pos <- getPosition
                        p <- AB.tagged $ liftM TokPunctuation (char '@')
                        s <- balancedBetween '(' ')'
                        liftM (p:) (subAtomo pos ("(" ++ s ++ ")"))
                    ]

                -- quoted expr
                , do
                    pos <- getPosition
                    p <- AB.tagged $ liftM TokPunctuation (oneOf "`'~")
                    s <- balancedBetween '(' ')'
                    liftM (p:) (subAtomo pos ("(" ++ s ++ ")"))

                -- some atomo code between keywords
                , do
                    p <- getPosition
                    s <- manyTill anyToken $ choice
                        [ lookAhead . try $ do
                            AB.anyIdent
                            char ':'
                            return ()
                        , lookAhead (space >> return ())
                        , eof
                        ]

                    subAtomo p s
                ]

            -- single reference; trailing punctuation is ignored
            , do
                l@(TaggedToken { tToken = TokIdentifier ident })
                    <- AB.tagged AL.lIdentifier

                let punct = reverse . takeWhile AB.isOpLetter . reverse $ ident
                    sane = reverse . dropWhile AB.isOpLetter . reverse $ ident

                getInput >>= setInput . (punct ++)

                return (ATokAtomo [l { tToken = TokIdentifier sane }])
            ]
        dump ("got value", val)
        return (name, val)

    let (ns, vs) = unzip ks

    return (ATokKeyword ns vs)

single :: Lexer AToken
single = fmap (debug "single") $ do
    char special
    name <- AB.identifier
    notFollowedBy (char ':')
    dump ("got single identifier", name)
    return (ATokSingle name)

atomo :: Lexer AToken
atomo = fmap (debug "atomo") $ do
    char special
    fmap ATokAtomo nestedAtomo

lexer :: Lexer [TaggedAToken]
lexer = do
    ss <- many . tagged $ choice
        [ try keyword
        , try single
        , try atomo
        , chunk
        ]
    eof
    return ss

tagged :: Lexer AToken -> Lexer TaggedAToken
tagged x = do
    p <- getPosition
    r <- x
    return (TaggedAToken r p)

defLexer :: Lexer AToken
defLexer = do
    thumb <- atomoBit

    AB.whiteSpace

    cs <- many . try $ do
        AB.symbol "|"
        d <- atomoBit
        AB.whiteSpace
        return d

    AB.whiteSpace

    ret <- AB.symbol ">" >> atomoBit

    return (ATokDefinition thumb cs ret)
  where
    atomoBit = do
        p <- getPosition
        manyTill anyToken (eol <|> eof) >>= subAtomo p

    eol = newline >> return ()

-- grab text between characters, balanced
balancedBetween :: Char -> Char -> Lexer String
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
