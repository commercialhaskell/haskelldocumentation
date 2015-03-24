{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad

import Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F
import Data.List ((\\))
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)
import qualified Data.Traversable as T
import Data.Typeable (Typeable)
import qualified Data.Yaml as Yaml

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hAuthorization, hAccept, Header)
import Network.HTTP.Types.Status (statusCode)

import System.Directory (setCurrentDirectory, getDirectoryContents)
import System.Environment (getArgs)

import qualified Text.HTML.DOM as Html
import qualified Text.XML as Html hiding (parseLBS)
import qualified Text.XML.Cursor as Html

import qualified Text.Regex.PCRE.Light as Regex
import Preprocess (preprocessMarkdown)


defaultHost :: Text
defaultHost = "https://www.fpcomplete.com/"

configFileName :: String
configFileName = "soh-upload.yaml"

data Context = Context {
  userAuthorization :: ByteString,
  host :: Text,
  folder :: Text,
  userPrefix :: Text
} deriving Show

data File = File {
  fileName :: Text,
  fileExtension :: Text
} deriving Show

data SaveConfig = SaveConfig {
  concurrencyToken :: ByteString
} deriving Show

data Metadata a = Metadata {
  mdCsrfToken :: a,
  mdTitle :: a,
  mdDescription :: a,
  mdSlug :: a,
  mdPackageSet :: a
} deriving (Functor, F.Foldable, T.Traversable, Show, Eq, Typeable)

data HtmlParseException a
  = FailedParseOf Text
  | AmbiguousParseOf Text [a]
  deriving (Show, Typeable)

type Slug = Text


htmlParseOne :: (MonadThrow m, Typeable a, Show a)
  => Text -> [a] -> m a
htmlParseOne thing = either throwM return . go where
  -- Goes through Either to help the type system.
  go []    = Left $ FailedParseOf thing
  go [x]   = Right x
  go xs    = Left $ AmbiguousParseOf thing xs

instance (Show a, Typeable a) => Exception (HtmlParseException a)

instance Applicative Metadata where
  pure a = Metadata {
    mdCsrfToken   = a,
    mdTitle       = a,
    mdDescription = a,
    mdSlug        = a,
    mdPackageSet  = a
  }

  mf <*> ma = Metadata {
    mdCsrfToken   = mdCsrfToken mf   $ mdCsrfToken ma,
    mdTitle       = mdTitle mf       $ mdTitle ma,
    mdDescription = mdDescription mf $ mdDescription ma,
    mdSlug        = mdSlug mf        $ mdSlug ma,
    mdPackageSet  = mdPackageSet mf  $ mdPackageSet ma
  }

newtype MetadataUpdate = MetadataUpdate (Metadata (Maybe Text))
  deriving Show

instance FromJSON MetadataUpdate where
  parseJSON = withObject "MetadataUpdate" $ \o -> do
    title <- o .:? "title"
    description <- o .:? "description"
    return $ MetadataUpdate $ (pure Nothing) {
      mdTitle = title,
      mdDescription = description
    }

updateMetadata :: Metadata a -> Metadata (Maybe a) -> Metadata a
updateMetadata = liftA2 fromMaybe

metadataEq :: Eq a => Metadata a -> Metadata a -> Bool
metadataEq m1 m2
  = mdTitle m1 == mdTitle m2
 && mdDescription m1 == mdDescription m2
 && mdSlug m1 == mdSlug m2
 && mdPackageSet m1 == mdPackageSet m2
 -- mdCsrfToken omitted intentionally

-- TODO: deal with dumb slashes
instance FromJSON Context where
  parseJSON = withObject "Context" $ \o -> do
    securityToken <- o .: "security-token"
    let userAuthorization = T.encodeUtf8 ("token " <> securityToken)
    user <- o .: "user"
    let userPrefix = "user/" <> user <> "/"
    host <- o .:? "host" .!= defaultHost
    folder <- o .:? "folder" .!= ""
    return Context{..}


getContext :: IO Context
getContext = do
  getArgs >>= \case
    (dir:_) -> setCurrentDirectory dir
    _ -> return ()
  Yaml.decodeFile configFileName >>= \case
    Nothing -> error $ "Failed to parse " <> configFileName
    Just context -> return context

-- Requests

parseWithAuth :: Context -> Text -> [Header] -> IO Request
parseWithAuth Context{..} path headers = do
  req <- parseUrl $ T.unpack $ host <> path
  return req {
    requestHeaders
      = (hAuthorization, userAuthorization)
      : headers
  }


getListReq :: Context -> IO Request
getListReq context@Context{..} = do
  let path = userPrefix <> folder
  parseWithAuth context path []

getTutorialReq :: Context -> Slug -> IO Request
getTutorialReq context@Context{..} slug = do
  let path = "tutorial-edit/" <> folder <> slug
  req <- parseWithAuth context path []
  return $ req {
    cookieJar = Just (createCookieJar [])
  }


postNewTutorialReq :: Context -> IO Request
postNewTutorialReq context@Context{..} = do
  let path = "new-tutorial/" <> folder
  req <- parseWithAuth context path []
  return $ urlEncodedBody [] req

postMetadataReq :: Context -> Slug -> CookieJar -> Metadata Text -> IO Request
postMetadataReq context slug cookieJar metadata = do
  req <- getTutorialReq context slug
  let body = reqBodyFromMetadata metadata
  return $ urlEncodedBody body req {
    cookieJar = Just cookieJar
  }

postSaveReq :: Context -> Slug -> SaveConfig -> ByteString -> IO Request
postSaveReq context@Context{..} slug SaveConfig{..} content = do
  let path = "tutorial-save/" <> folder <> slug
  req <- parseWithAuth context path [(hAccept, "application/json")]
  let body =
        [ ("content", content)
        , ("token", concurrencyToken)
        ]
  return $ urlEncodedBody body req

postPublishReq :: Context -> Slug -> IO Request
postPublishReq context@Context{..} slug = do
  let path = "tutorial-publish/" <> folder <> slug
  req <- parseWithAuth context path []
  return $ urlEncodedBody [] req

postDelReq :: Context -> Slug -> IO Request
postDelReq context@Context{..} slug = do
  let path = "delete-content/" <> folder <> slug
  req <- parseWithAuth context path []
  return $ urlEncodedBody [("confirm", "")] req


markdownRegex :: Regex.Regex
markdownRegex = Regex.compile "^([a-zA-Z0-9\\-]+)(\\.md|\\.markdown)$" []

toFile :: String -> Maybe File
toFile fStr = case Regex.match markdownRegex fBS [] of
  Just [_, fileNameBS, fileExtensionBS] ->
    Just $ File {
      fileName = T.decodeUtf8 fileNameBS,
      fileExtension = T.decodeUtf8 fileExtensionBS
    }
  _ -> Nothing
  where fBS = T.encodeUtf8 $ T.pack fStr

getFiles :: IO [File]
getFiles = mapMaybe toFile <$> getDirectoryContents "."

-- TODO: make this lazier?
parseFrontmatter :: ByteString -> Either String (MetadataUpdate, ByteString)
parseFrontmatter bs = yamlBSEither >>= decodeEitherFst where
  decodeEitherFst (x, s) = case Yaml.decodeEither x of
    Left e -> Left e
    Right y -> Right (y, s)
  ts = T.lines $ T.decodeUtf8 bs
  isYamlMarker = (== "---")
  yamlBSEither = case span isYamlMarker ts of
    ([_], ts') -> case span (not . isYamlMarker) ts' of
      ([], _) -> Left "No yaml found"
      (_, []) -> Left "Ending yaml marker not found"
      (yamlLines, (_:mdLines)) ->
        let yamlBS = T.encodeUtf8 $ T.unlines yamlLines
            mdBS = T.encodeUtf8 $ T.unlines mdLines
        in Right (yamlBS, mdBS)
    _ -> Left "Begining yaml marker not at beginning"


metadataUpdatesFromBS :: ByteString -> (Metadata (Maybe Text), ByteString)
metadataUpdatesFromBS bs = case parseFrontmatter bs of
  Left e -> (pure Nothing, bs) -- TODO: log errors?
  Right (MetadataUpdate m, bs') -> (m, bs')

readFileBS :: File -> IO ByteString
readFileBS File{..} = BS.readFile $ T.unpack $ fileName <> fileExtension

saveUpdate :: Context -> File -> Manager -> SaveConfig -> IO ()
saveUpdate context file@File{..} manager saveConfig = do
  content <- readFileBS file
  let (metadataUpdates, content') = metadataUpdatesFromBS content
  content'' <- preprocessMarkdown content'
  req <- postSaveReq context fileName saveConfig content''
  response <- httpLbs req manager
  setMetadata context fileName metadataUpdates manager

createNew :: Context -> Slug -> Manager -> IO SaveConfig
createNew context slug manager = do
  response <- newTutorial context manager

  -- TODO: avoid the extra back and forth
  oldMetadata <- htmlParseMetadata (responseBody response)
  let metadataUpdates = (pure Nothing) { mdSlug = Just slug }
      oldSlug = mdSlug oldMetadata
  setMetadata context (mdSlug oldMetadata) metadataUpdates manager

  tokenText <- htmlParseConcurrencyToken (responseBody response)
  T.putStrLn $ "Created new document: " <> slug
  return $ SaveConfig {
    concurrencyToken = T.encodeUtf8 tokenText
  }

newTutorial :: Context -> Manager -> IO (Response LBS.ByteString)
newTutorial context manager = do
  req <- postNewTutorialReq context
  httpLbs req manager


reqBodyFromMetadata :: Metadata Text -> [(ByteString, ByteString)]
reqBodyFromMetadata m =
  [ ("_token", mdCsrfToken)
  , ("f1", mdTitle)
  , ("f2", mdDescription)
  , ("f3", mdSlug)
  , ("f4", mdPackageSet)
  ] where Metadata{..} = fmap T.encodeUtf8 m

testNewTutorial :: IO ()
testNewTutorial = do
  context <- getContext
  manager <- newManager tlsManagerSettings
  print =<< newTutorial context manager


testGetMetadata :: IO ()
testGetMetadata = do
  context <- getContext
  manager <- newManager tlsManagerSettings
  let slug = "upload-test"
  (_, m) <- getMetadata context slug manager
  print m

testSetMetadata :: IO ()
testSetMetadata = do
  context <- getContext
  manager <- newManager tlsManagerSettings
  let slug = "upload-test"
  let updatesToMetadata = (pure Nothing) {
    mdTitle = Just "Title",
    mdDescription = Just "Desc",
    mdSlug = Just "upload-test"
  }

  setMetadata context slug updatesToMetadata manager
  (_, m) <- getMetadata context slug manager
  --putStr "After: " >> print m
  return ()


getMetadata :: Context -> Slug -> Manager -> IO (CookieJar, Metadata Text)
getMetadata context slug manager = do
  reconReq <- getTutorialReq context slug
  reconResponse <- httpLbs reconReq manager
  metadata <- htmlParseMetadata (responseBody reconResponse)
  return (responseCookieJar reconResponse, metadata)


setMetadata :: Context -> Slug -> Metadata (Maybe Text) -> Manager -> IO ()
setMetadata context slug updatesToMetadata manager = do
  (cookieJar, oldMetadata) <- getMetadata context slug manager
  let newMetadata = updateMetadata oldMetadata updatesToMetadata

  req <- postMetadataReq context slug cookieJar newMetadata
  response <- httpLbs req manager

  m <- htmlParseMetadata (responseBody response)
  if metadataEq m newMetadata
    then return () -- putStrLn $ "Metadata updated successfully." >> print m
    else fail ("Metadata didn't update.\n"
      <> "Should be: " <> show newMetadata
      <> "Found: " <> show m )



htmlParseConcurrencyToken :: MonadThrow m => LBS.ByteString -> m Text
htmlParseConcurrencyToken lbs = htmlParseOne "concurrency token" parsed where
  parsed = cursor Html.$// selector
  cursor = Html.fromDocument (Html.parseLBS lbs)
  selector = Html.attributeIs "id" "content"
         >=> Html.attribute "data-concurrent"


htmlParseMetadata :: MonadThrow m => LBS.ByteString -> m (Metadata Text)
htmlParseMetadata lbs = htmlParseOne "metadata" parsed where
  parsed = T.sequenceA Metadata{..}
  html = Html.fromDocument (Html.parseLBS lbs)
  formSelector = Html.element "form" >=> Html.attributeIs "enctype" "application/x-www-form-urlencoded"

  mdCsrfToken   = html Html.$// formSelector Html.&// csrfTokenSelector
  mdTitle       = html Html.$// formSelector Html.&// titleSelector 
  mdDescription = html Html.$// formSelector Html.&// descriptionSelector
  mdSlug        = html Html.$// formSelector Html.&// slugSelector
  mdPackageSet  = html Html.$// formSelector Html.&// packageSelector

  csrfTokenSelector = inputValOf $ Html.attributeIs "name" "_token"
  titleSelector = inputValOf $ Html.attributeIs "name" "f1"
  descriptionSelector = textareaValOf $ Html.attributeIs "name" "f2"
  slugSelector = inputValOf $ Html.attributeIs "name" "f3"
  packageSelector = selectValOf $ Html.attributeIs "name" "f4"

  inputValOf selector = Html.element "input" >=> selector
                        >=> Html.attribute "value"
  textareaValOf selector = Html.element "textarea" >=> selector
                           >=> childContentOr ""
  selectValOf selector =
    Html.element "select" >=> selector
      Html.&/ Html.element "option" >=> Html.hasAttribute "selected"
      >=> Html.attribute "value"

  childContentOr def c = case c Html.$/ Html.content of
    [] -> pure def
    res -> res


-- Discovers the concurrency token.
-- Creates the tutorial first if it doesn't exist yet.
getSaveConfig :: Context -> Slug -> Manager -> IO SaveConfig
getSaveConfig context slug manager = do
    req <- getTutorialReq context slug

    handle handler404 $ do
      response <- httpLbs req manager
      tokenText <- htmlParseConcurrencyToken (responseBody response)
      return $ SaveConfig {
        concurrencyToken = T.encodeUtf8 tokenText
      }
  where
    handler404 e@(StatusCodeException status _ _) =
      if statusCode status == 404
      then createNew context slug manager
      else throwIO e

publishTutorial :: Context -> Slug -> Manager -> IO ()
publishTutorial context slug manager = do
  req <- postPublishReq context slug
  response <- httpLbs req manager
  -- TODO: check the response?
  return ()

uploadFile :: Context -> Manager -> File -> IO ()
uploadFile context manager file@File{..} = do
  T.putStrLn $ "Uploading: " <> fileName
  getSaveConfig context fileName manager >>=
    saveUpdate context file manager
  publishTutorial context fileName manager


uploadAll :: Context -> [File] -> Manager -> IO ()
uploadAll context files manager = do
  putStrLn $ "Uploading " <> show (length files) <> " files."
  forM_ files $ uploadFile context manager
  putStrLn "All files uploaded."

deleteItem :: Context -> Manager -> Slug -> IO ()
deleteItem context manager slug = do
  T.putStrLn $ "Deleting: " <> slug
  req <- postDelReq context slug
  httpNoBody req manager
  return ()


deleteAllExceptFor :: Context -> [File] -> Manager -> IO ()
deleteAllExceptFor context@Context{..} files manager = do
    req <- getListReq context
    lbs <- responseBody <$> httpLbs req manager

    let cursor = Html.fromDocument (Html.parseLBS lbs)
        links = cursor Html.$// selector
        slugs = map parseSuffix links \\ map fileName files

    putStrLn $ "Deleting " <> show (length slugs) <> " items from SoH"
    forM_ slugs $ deleteItem context manager
    putStrLn "Deletions complete."
  where
    prefix = host <> userPrefix <> folder

    selector = Html.element "ul" >=> hasClass "media-list"
       Html.&/ Html.element "li"
      Html.&// Html.element "a" >=> checkAttribute "href" pred
           >=> Html.attribute "href"
      where
        pred = T.isPrefixOf prefix

    hasClass c = Html.checkElement $ \e ->
      case M.lookup "class" (Html.elementAttributes e) of
        Nothing -> False
        Just cs -> any (== c) (T.words cs)

    checkAttribute attr pred = Html.checkElement $ \e ->
      case M.lookup attr (Html.elementAttributes e) of
        Nothing -> False
        Just v -> pred v

    parseSuffix link =
      let Just suffix = T.stripPrefix prefix link
      in suffix


main = do
  context <- getContext
  files <- getFiles
  withManager tlsManagerSettings $ \manager -> do
    uploadAll context files manager
    deleteAllExceptFor context files manager
