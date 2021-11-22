{-# LANGUAGE OverloadedStrings #-}
module PdfTemplate.Worker
where

import Data.Function ((&))
import Data.Bifunctor(first)
import Data.Functor ((<&>))
import Control.Monad (forM_)
import Data.Aeson (Value(..), object, (.=))
import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified System.FilePath as Path
import Text.Mustache (compileMustacheText, renderMustache, MustacheException (..))
import Control.Exception (throwIO)
import qualified Data.Text.Lazy as Text

process :: FilePath -> IO ()
process path = do
  archive <- Zip.toArchive <$> LBS.readFile path
  let files = Zip.zEntries archive
  putStrLn $ "Found " ++ show (length files) ++ " files"
  forM_ files $ \file -> do
    let fullName = Zip.eRelativePath file
    let (_, ext) = Path.splitExtension fullName

    putStrLn $ "Processing " ++ fullName

    let xxx = object
                [ "Title"   .= ("John" :: Text)
                , "things" .= ["pen" :: Text, "candle", "egg"]
                ]

    let newContent =
          if ext `elem` [ ".htm", ".html", ".css" ]
            then unTemplateBS xxx $ Zip.fromEntry file
            else Right $ Zip.fromEntry file

    either throwIO LBS.putStrLn newContent

unTemplateBS :: Value -> LBS.ByteString -> Either MustacheException LBS.ByteString
unTemplateBS params txt =
  txt
    &   LBS.toStrict
    &   Text.decodeUtf8
    &   unTemplate params
    <&> Text.encodeUtf8
    <&> LBS.fromStrict

unTemplate :: Value -> Text -> Either MustacheException Text
unTemplate params txt =
  compileMustacheText "foo" txt
    &   first MustacheParserException
    <&> flip renderMustache params
    <&> Text.toStrict

