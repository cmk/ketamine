{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Formatting of log records acc. to <logfmt http://brandur.org/logfmt>
--
-- This format enjoys popularity among Golang users as well as log aggregation
-- SaaS providers, as it strikes a balance between human and machine
-- readability. One of it's advantages over other encodings of structured log
-- records is that parsers can gracefully handle unstructured output (eg. from
-- 3rd party software).
module Numeric.Ketamine.Log.Logfmt (
    formatLogfmt
  , parseLogfmt
  , fmtString
  ) where


import Control.Applicative
import Data.Bifunctor
import Data.Foldable (foldr')
import qualified Data.Aeson.Encoding as Enc
import           Data.Aeson.Parser (jstring)
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import           Data.Char (isAlpha, isDigit)
import qualified Data.HashSet as Set
import           Data.List ((++))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy.Encoding as LTE


import           Numeric.Ketamine.Log.Types


-- | Format a log record acc. to the \"logfmt\" convention
--
-- Encoding is slightly more robust / parser-friendly than other implementations
-- in that:
--
--  * strings are always quoted (and properly escaped)
--  * keys of key/value pairs restricted to the character set @[._a-zA-Z0-9]@
--    (invalid characters are silently replaced by the underscore character).
--
-- By convention, the reserved keys @level, msg, loc@ exist.  Name clashes are
-- resolved by prefixing with \"field.\".
formatLogfmt ::
     IsLogStr msg
  => Severity
  -> Maybe Loc
  -> LogContext
  -> msg
  -> LogStr
formatLogfmt sev loc ctx msg =
  let
    possibly logstr
      | logStrLength logstr > 0 = logstr <> space
      | otherwise               = mempty
  in
    mconcat [
        fmtSev sev
      , space
      , fmtMsg msg
      , space
      , possibly (fmtCtx ctx)
      , maybe mempty fmtLoc loc
      ]


-- | Simplistic parser for \"logfmt\" encoded strings
--
-- Mainly useful for testing.
--
-- >>> parseOnly parseLogfmt "Banana pie level=\"info\" trailing garbage"
-- Right [Left "Banana pie",Right ("level",StringV "info"),Left "trailing garbage"]
--
parseLogfmt :: Parser [Either ByteString LogField]
parseLogfmt =
  let
    garbage =
      Left <$> P.takeWhile1 (/= ' ')
    kv =
      fmap Right . (,) <$> key <*> val

    key =
      fmap TE.decodeUtf8 $
        P.skipSpace *> P.takeWhile1 (P.inClass "._a-zA-Z0-9") <* P.char '='
    val =
      str <|> num <|> bool'

    str =
      StringV <$> jstring
    num =
      NumV <$> P.scientific
    bool' =
      BoolV <$> (const True <$> P.string "true" <|> const False <$> P.string "false")

    -- 'mappend' successive 'Left's in the input. Should probably use
    -- 'Validation', modulo the whitespace separator
    catlefts = uncurry (:) . foldr' go (Right $ error "catlefts", [])
      where
        go (Left  g) (Left g', acc) = (Left (g <> " " <> g'), acc)
        go (Right h) (Left  g, acc) = (Right h, Right h:Left g:acc)
        go (Left  g) (Right _, acc) = (Left  g, acc)
        go (Right h) (Right _, acc) = (Right h, Right h:acc)

  in
    catlefts <$> (kv <|> garbage) `P.sepBy` P.char ' '


------------------------------------------------------------------------------

fmtSev :: Severity -> LogStr
fmtSev =
  pair "level" . quote . severityToLogStr

fmtLoc :: Loc -> LogStr
fmtLoc Loc{..} =
  let
    loc = loc_package ++ ':' : loc_module ++ ':' : line ++ ':' : char
    (line,char) = bimap show show loc_start
  in
    pair "loc" (fmtString loc)

fmtCtx :: LogContext -> LogStr
fmtCtx =
  let
    go b k v
        = b <> bool mempty space (logStrLength b > 0)
        <> pair (fmtKey k) (fmtVal v)

    fmtKey
        = toLogStr
        . (\k -> bool k ("field." <> k) $ Set.member k reservedKeys)
        . T.map (\c -> bool '_' c $ safeKey c)

    safeKey c = c == '.' || c == '_' || isAlpha c || isDigit c

    reservedKeys = Set.fromList ["level", "loc", "msg"]
  in
    foldContext go mempty

fmtVal :: LogFieldValue -> LogStr
fmtVal = \case
  NumV v ->
    fromBuilder . Enc.fromEncoding . Enc.scientific $ v
  BoolV v ->
    if v then "true" else "false"
  StringV v ->
    fmtString v

fmtMsg :: IsLogStr msg => msg -> LogStr
fmtMsg =
  pair "msg" . fmtString

fmtString :: IsLogStr s => s -> LogStr
fmtString =
  let
    enc (LogStrS s) = Enc.string s
    enc (LogStrT s) = Enc.text s
    enc (LogStrLT s) = Enc.lazyText s
    enc (LogStrBS s) = Enc.text . TE.decodeUtf8With lenientDecode $ s
    enc (LogStrLBS s) = Enc.lazyText . LTE.decodeUtf8With lenientDecode $ s
  in
    fromBuilder . Enc.fromEncoding . enc . logStr

pair :: LogStr -> LogStr -> LogStr
pair k v =
  k <> "=" <> v

quote :: LogStr -> LogStr
quote s =
  "\"" <> s <> "\""

space :: LogStr
space =
  " "

fromBuilder :: Builder -> LogStr
fromBuilder =
  toLogStr . toLazyByteString
