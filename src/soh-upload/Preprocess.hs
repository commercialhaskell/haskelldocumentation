{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Preprocess (preprocessMarkdown) where

import Data.Maybe
import Control.Applicative
import Data.Conduit
import Data.Conduit.Binary (sinkLbs)
import qualified Data.Conduit.List as CL (map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.XML.Types (Event (EventBeginElement, EventContent), Content (ContentText), Name, Content)
import Text.HTML.DOM (eventConduit)
import Text.XML.Stream.Render (renderBytes, def)
import Text.Markdown (markdown)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Data.Conduit.Blaze (builderToByteString)
import qualified Data.Text.Lazy.Encoding as LT
import Data.Monoid


hrefTweakEvent :: Event -> Event
hrefTweakEvent (EventBeginElement "a" attrs) =
  EventBeginElement "a" (hrefTweakAttrs attrs)
hrefTweakEvent e = e

hrefTweakAttrs :: [(Name, [Content])] -> [(Name, [Content])]
hrefTweakAttrs = map tweakIfHref where
  tweakIfHref ("href", hrefs) = ("href", map tweakContent hrefs)
  tweakIfHref a = a
  tweakContent (ContentText href) = ContentText $ hrefTweak href
  tweakContent c = c

type Href = Text

-- Strip the .md or .markdown suffix from relative hrefs.
hrefTweak :: Href -> Href
hrefTweak href | isAbsolute href = href
hrefTweak href = fromMaybe href maybeStripped where
  maybeStripped
      = T.stripSuffix ".md" href
    <|> T.stripSuffix ".markdown" href

isAbsolute :: Href -> Bool
isAbsolute = T.isInfixOf "//"


-- If the first line of a Haskell code block is "-- active"
-- then make it an Active Haskell code block.
activeHaskellTweak :: Monad m => Conduit Event m Event
activeHaskellTweak = awaitForever $ \e1 -> case e1 of
    EventBeginElement "code" attrs -> case lookup "class" attrs of
      Just [ContentText "haskell"] -> await >>= \case
        Just e2@(EventContent (ContentText t)) -> case T.lines t of
          ("-- active":ts) -> do
    -- Success case: yield modified elements
            yield $ EventBeginElement "code" $ withActiveHaskell attrs
            yield $ EventContent $ ContentText $ T.unlines ts
    -- Failure cases: yield whatever has been consumed so far
          _     -> yield e1 >> yield e2
        Just e2 -> yield e1 >> yield e2
        Nothing -> yield e1
      _         -> yield e1
    _           -> yield e1
  where
    withActiveHaskell = map activeHaskellClass
    activeHaskellClass ("class", _) = ("class", [ContentText "haskell active"])
    activeHaskellClass attr = attr


-- This is the part of the pipeline
-- that performs modifications to XML events.
-- Any future modifications can be fused in here.
eventModifications :: Monad m => Conduit Event m Event
eventModifications = CL.map hrefTweakEvent =$= activeHaskellTweak


preprocessMarkdown :: ByteString -> IO ByteString
preprocessMarkdown bs = fmap afterStream $ asIO
   $ yield (renderHtmlBuilder renderedMarkdown)
  $= builderToByteString
  $= eventConduit
  $= eventModifications
  $= renderBytes def
  $$ sinkLbs

  where
    afterStream lbs = LBS.toStrict ("<!DOCTYPE html>\n" <> lbs)
    renderedMarkdown = markdown def lmd
    lmd = LT.decodeUtf8 $ LBS.fromStrict bs

    asIO :: IO a -> IO a
    asIO = id
