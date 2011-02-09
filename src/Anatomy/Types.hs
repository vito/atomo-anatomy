{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}
module Anatomy.Types where

import Control.Monad.State
import Data.IORef
import Data.Typeable
import Text.Parsec (SourcePos)

import Atomo.Lexer.Base (TaggedToken)
import qualified Atomo.Types as AT

data Title =
    Title
        { titleText :: Segment
        , titleTag :: Maybe Segment
        , titleVersion :: Maybe Segment
        }
    deriving (Show, Typeable)

data Style
    = None
    | TOC
    | Unnumbered
    | Annotated
    | Slides
    | Class String
    | Styles [Style]
    deriving (Eq, Show, Typeable)

data BindingKey
    = KeywordKey [String]
    | SingleKey String
    deriving (Eq, Ord, Show, Typeable)

data Definition =
    Definition
        { defThumb :: AT.Expr
        , defContracts :: [AT.Expr]
        , defReturn :: AT.Expr
        }
    deriving Show

data Section =
    Section
        { sectionID :: SectionRef
        , sectionTitle :: Title
        , sectionStyle :: Style    -- style flags (e.g. TOC)
        , sectionBody :: [Segment] -- un-built content with parts from step 1 removed
        , sectionBindings :: [BindingKey]
        , sectionParent :: Maybe SectionRef
        , subSections :: [Section] -- included sections
        , sectionDepth :: Int
        , sectionNumber :: Int
        , sectionPath :: FilePath
        , sectionA :: AT.Value
        }
    deriving (Show, Typeable)

type SectionRef = IORef Section

instance Show SectionRef where
    show _ = "SectionRef"

type AVM = StateT Section AT.VM

data Segment
    = Chunk String
    | KeywordDispatch [String] [Segment]
    | SingleDispatch String
    | Atomo AT.Expr
    | Nested [Segment]
    | SectionReference Int
    | TableOfContents
    | FullTableOfContents
    | InlineDefinition Definition (Maybe Segment)
    deriving Show

data TOCTree
    = Node String String
    | Branch String String [TOCTree]
    deriving Show

data AToken
    = ATokChunk String
    | ATokKeyword [String] [TaggedAToken]
    | ATokSingle String
    | ATokAtomo [TaggedToken]
    | ATokNested [TaggedAToken]
    | ATokDefinition [TaggedToken] [[TaggedToken]] [TaggedToken]
    deriving Show

data TaggedAToken =
    TaggedAToken
        { taToken :: AToken
        , taLocation :: SourcePos
        }
    deriving Show

styleToClass :: Style -> String
styleToClass (Class s) = s
styleToClass TOC = "table-of-contents"
styleToClass Unnumbered = "unnumbered"
styleToClass Annotated = "annotated"
styleToClass Slides = "slides"
styleToClass None = "normal"
styleToClass (Styles ss) = unwords (map styleToClass ss)

addStyle :: Style -> Style -> Style
addStyle a None = a
addStyle (Styles a) (Styles b) = Styles (a ++ b)
addStyle a (Styles b) = Styles (a:b)
addStyle (Styles a) b = Styles (a ++ [b])
addStyle a b = Styles [a, b]

styleMatch :: Style -> Style -> Bool
styleMatch a b | a == b = True
styleMatch a (Styles b) = any (styleMatch a) b
styleMatch _ _ = False

defKey :: Definition -> BindingKey
defKey d =
    case defThumb d of
        AT.EDispatch { AT.eMessage = AT.Keyword { AT.mNames = ns } } -> KeywordKey ns
        AT.EDispatch { AT.eMessage = AT.Single { AT.mName = n } } -> SingleKey n
        _ -> error $ "no defKey for: " ++ show (defThumb d)

bindingID :: BindingKey -> String
bindingID (KeywordKey ns) = "definition_" ++ concatMap (++ ":") ns
bindingID (SingleKey n) = "definition_" ++ n

runAVM :: AVM a -> Section -> AVM a
runAVM a s = lift (evalStateT a s)

runAVM' :: AVM a -> Section -> AT.VM a
runAVM' = evalStateT

newSection :: (Section -> Section) -> IO Section
newSection f = do
    r <- newIORef undefined
    writeIORef r (sec r)
    return (sec r)
  where
    sec r = f Section
        { sectionID = r
        , sectionTitle = Title (Chunk "Untitled") Nothing Nothing
        , sectionStyle = None
        , sectionBody = []
        , sectionBindings = []
        , sectionParent = Nothing
        , subSections = []
        , sectionDepth = 0
        , sectionNumber = 1
        , sectionPath = ""
        , sectionA = error "no section anatomy"
        }


