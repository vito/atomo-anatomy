module Anatomy.Parser (parseFile, parseDefinition) where

import Control.Monad.Error
import Text.Parsec hiding (parse)
import Atomo.Parser (continue)
import Atomo.Lexer.Base (TaggedToken)
import Atomo.Parser.Expand
import Atomo.Parser.Expr hiding (parser)
import qualified Atomo.Types as AT
import qualified Atomo.Parser.Base as AP

import Anatomy.Lexer (lexer, defLexer, tagged)
import Anatomy.Parser.Base
import Anatomy.Types


pNested :: Parser Segment
pNested = do
    pos <- getPosition
    s <- getState
    ts <- nested
    case runParser p s (show pos) ts of
        Left e -> fail ("nested: " ++ show e)
        Right (r, s') -> do
            putState s'
            return (Nested r)
  where
    p = do
        r <- parser
        s <- getState
        return (r, s)

subParse :: [TaggedAToken] -> Parser [Segment]
subParse ts = do
    pos <- getPosition
    s <- getState
    case runParser p s (show pos) ts of
        Left e -> fail ("sub: " ++ show e)
        Right (r, s') -> do
            putState s'
            return r
  where
    p = do
        r <- parser
        s <- getState
        return (r, s)

pChunk :: Parser Segment
pChunk = do
    c <- chunk
    return (Chunk c)

pKeyword :: Parser Segment
pKeyword = do
    (ns, ts) <- keyword
    ss <- subParse ts
    return (KeywordDispatch ns ss)

pSingle :: Parser Segment
pSingle = fmap SingleDispatch single

pAtomo :: Parser Segment
pAtomo = do
    ts <- atomo
    fmap Atomo (subAtomo pExpr ts)

subAtomo :: AP.Parser a -> [TaggedToken] -> Parser a
subAtomo p ts = do
    pos <- getPosition
    s <- getState
    case runParser p s (show pos) ts of
        Left e -> fail ("atomo: " ++ show e)
        Right ok -> return ok

pDefinition :: Parser Definition
pDefinition = do
    (t, cs, r) <- definition
    nt <- subAtomo pDispatch t
    ncs <- mapM (subAtomo pDispatch) cs
    nr <- subAtomo pDispatch r
    return (Definition nt ncs nr)

parser :: Parser [Segment]
parser = do
    ss <- many $ choice
        [ try pKeyword
        , try pSingle
        , try pAtomo
        , try pNested
        , pChunk
        ]
    eof
    return ss

expandSegment :: Segment -> AT.VM Segment
expandSegment (Atomo e) = liftM Atomo (macroExpand e)
expandSegment (Nested ss) =
  liftM Nested (mapM expandSegment ss)
expandSegment (InlineDefinition d (Just s)) =
  liftM (InlineDefinition d . Just) (expandSegment s)
expandSegment s = return s

parseFile :: String -> AT.VM [Segment]
parseFile fn = liftIO (readFile fn) >>= continue lexer parser fn >>= mapM expandSegment

parseDefinition :: String -> AT.VM Definition
parseDefinition = continue
    (fmap (:[]) $ tagged defLexer)
    (do { r <- pDefinition; eof; return r })
    "<definition>"
