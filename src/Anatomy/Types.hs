{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}
module Anatomy.Types where

import Control.Monad.State
import Data.IORef
import Data.Typeable

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
    | The AT.Expr
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

defKey :: Definition -> BindingKey
defKey d =
    case defThumb d of
        AT.Dispatch { AT.eMessage = AT.EKeyword { AT.emNames = ns } } -> KeywordKey ns
        AT.Dispatch { AT.eMessage = AT.ESingle { AT.emName = n } } -> SingleKey n

bindingID :: BindingKey -> String
bindingID (KeywordKey ns) = "definition_" ++ concatMap (++ ":") ns
bindingID (SingleKey n) = "definition_" ++ n
