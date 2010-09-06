module Anatomy.AutoFlow where

import Data.Char (isSpace)
import Text.HTML.TagSoup

autoFlow :: String -> String
autoFlow = renderTags . autoFlow' False . canonicalizeTags . parseTags
  where
    autoFlow' open []
        | open = [TagClose "p"]
        | otherwise = []
    autoFlow' open (o@(TagOpen n _):ts)
        -- inline element, already in a <p>; carry on
        | isPhrasing n && open = o : autoFlow' open ts

        -- inline element, not in a <p>; start a <p>
        | isPhrasing n = TagOpen "p" [] : o : autoFlow' True ts
        
        -- opening a block element; </p>
        | open = TagClose "p" : autoFlow' False (o:ts)

        -- a block that can contain <p>
        | isFlowable n = o : autoFlow' False tagContents ++ [TagClose n] ++ autoFlow' False rest

        -- not <p>-able; leave it alone
        | otherwise = o : tagContents ++ [TagClose n] ++ autoFlow' False rest
      where
        tagContents = getTagContent n (o:ts)
        rest = drop (length tagContents + 1) ts
    autoFlow' open (t@(TagText c):ts)
        -- skip whitespace
        | all isSpace c = t : autoFlow' open ts

        -- a new paragraph, indicated by a linebreak
        | open && head c == '\n' = TagClose "p" : autoFlow' False (t:ts)

        -- a new paragraph, indicated by a linebreak
        | head c == '\n' = TagOpen "p" [] : autoFlow' True (TagText (dropWhile (== '\n') c) : ts)

        -- already open; carry on
        | open = thisPara : autoFlow' True rest

        -- text, pop it in a <p>
        | otherwise = TagOpen "p" [] : t : autoFlow' True ts
      where
        thisPara = TagText (takeWhile (/= '\n') c)
        rest
            | thisPara == t = ts
            | otherwise = TagText (dropWhile (/= '\n') c) : ts
    autoFlow' True (t@(TagClose n):ts)
        | isBlock n =
            TagClose "p" : t : autoFlow' False ts
    autoFlow' open (t:ts) = t : autoFlow' open ts

isPhrasing :: String -> Bool
isPhrasing = flip elem . words $
    "a abbr area audio b bdo br button canvas cite code command datelist del dfn em embed i iframe img input ins kbd keygen label link map mark math meta meter noscript object output progress q ruby samp script select small span strong sub sup svg textarea time var video wbr"

-- flow content, excluding phrasing
isBlock :: String -> Bool
isBlock = flip elem . words $
    "article aside blockquote details div dl fieldset figure footer form h1 h2 h3 h4 h5 h6 header hgroup hr menu nav ol p pre section style table ul"

isFlow :: String -> Bool
isFlow x = isPhrasing x || isBlock x

isFlowable :: String -> Bool
isFlowable = flip elem . words $
    "div"

getTagContent :: String -> [Tag String] -> [Tag String]
getTagContent n ts' = getTagContent' 0 [] ts'
  where
    getTagContent' :: Int -> [Tag String] -> [Tag String] -> [Tag String]
    -- error: reached the end with an unclosed tag
    getTagContent' _ _ [] = error $ "unmatched tag " ++ n ++ " in: " ++ show ts'

    -- final closing tag
    getTagContent' 1 acc (TagClose tn:_) | tn == n =
        reverse acc

    -- initial opening tag
    getTagContent' 0 acc (TagOpen tn _:ts) | tn == n =
        getTagContent' 1 acc ts

    -- nested open tag
    getTagContent' d acc (TagOpen tn as:ts) | tn == n =
        getTagContent' (d + 1) (TagOpen tn as:acc) ts

    -- nested close tag
    getTagContent' d acc (TagClose tn:ts) | tn == n =
        getTagContent' (d - 1) (TagClose tn:acc) ts

    -- content
    getTagContent' d acc (t:ts) =
        getTagContent' d (t:acc) ts
