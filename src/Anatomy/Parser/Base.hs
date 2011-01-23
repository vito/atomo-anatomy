module Anatomy.Parser.Base where

import Control.Monad.Identity
import Text.Parsec

import Atomo.Types (ParserState)
import Atomo.Lexer.Base (TaggedToken)

import Anatomy.Types


type Parser = ParsecT [TaggedAToken] ParserState Identity

withToken :: (AToken -> Maybe a) -> Parser a
withToken f =
    -- TODO:   showAToken
    tokenPrim (show . taToken) (\_ t _ -> taLocation t) (f . taToken)

chunk :: Parser String
chunk = withToken $ \t ->
    case t of
        ATokChunk s -> Just s
        _ -> Nothing

keyword :: Parser ([String], [TaggedAToken])
keyword = withToken $ \t ->
    case t of
        ATokKeyword ns ts -> Just (ns, ts)
        _ -> Nothing

single :: Parser String
single = withToken $ \t ->
    case t of
        ATokSingle n -> Just n
        _ -> Nothing

atomo :: Parser [TaggedToken]
atomo = withToken $ \t ->
    case t of
        ATokAtomo ts -> Just ts
        _ -> Nothing

nested :: Parser [TaggedAToken]
nested = withToken $ \t ->
    case t of
        ATokNested ts -> Just ts
        _ -> Nothing

definition :: Parser ([TaggedToken], [[TaggedToken]], [TaggedToken])
definition = withToken $ \t ->
    case t of
        ATokDefinition th cs r -> Just (th, cs, r)
        _ -> Nothing
