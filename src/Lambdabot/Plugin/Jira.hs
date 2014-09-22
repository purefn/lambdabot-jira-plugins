{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Lambdabot.Plugin.Jira (jiraPlugins, jiraPlugin) where

import Control.Applicative
import Control.Exception.Lifted as E (SomeException(..), catch)
import Control.Monad hiding (forM_)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Aeson (FromJSON(..), Value(..), Object, (.:), (.:?), eitherDecode)
import Data.Aeson.Types (Parser)
import Data.Array (elems)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as B
import Data.Foldable hiding (concat)
import Data.List (nub)
import Data.List.Split
import Data.Monoid
import Data.Text (Text)
import Lambdabot.IRC
import Lambdabot.Monad
import Lambdabot.Plugin
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Text.Regex.PCRE

jiraPlugins :: [String]
jiraPlugins = ["jira"]

data Jira = Jira
  { jiraUrl :: String
  , jiraUser :: String
  , jiraPass :: String
  }

type S = (Manager, [Jira])

jiraPlugin :: Module S
jiraPlugin = newModule
  { moduleCmds = return
      [ (command "jira")
          { aliases = []
          , help = say "jira <url> <username> <password>. Connect to HipChat."
          , process = jiraCommand
          }
      ]
  , moduleDefState = liftIO $ (, []) <$> newManager tlsManagerSettings
  , contextual = linkIssues
  }

jiraCommand :: String -> Cmd (ModuleT S LB) ()
jiraCommand xs = case splitOn " " xs of
  (url:u:p:_) ->
    catch (addJira $ Jira url u p) $ \e@SomeException{} ->
      say ("Connection failed. " ++ show e)
  _ -> say "Not enough parameters!"

getManager :: Cmd (ModuleT S LB) Manager
getManager = fst <$> readMS

addJira :: Jira -> Cmd (ModuleT S LB) ()
addJira j = getManager >>= tryReq >> add where
  tryReq m = liftIO $ do
    req <- newReq j "/rest/api/2/myself" $ \req' ->
      req' { method = "HEAD" }
    httpNoBody req m
  add = modifyMS ((j:) <$>)

linkIssues :: String -> Cmd (ModuleT S LB) ()
linkIssues s = readMS >>= forM_ (keys s) . sayLink where

regex :: Regex
regex = makeRegex ("(?<![^\\s(])(\\p{Lu}{2,}-\\d+)\\b" :: String)

keys :: String -> [String]
keys = nub . (fst <$>) . join . (elems <$>) . matchAllText regex

sayLink :: S -> String -> Cmd (ModuleT S LB) ()
sayLink (m, js) = (traverse_ sayLink' =<<) . runMaybeT . lookupI js where
  lookupI js' key = case js' of
    [] -> MaybeT $ return Nothing
    j:js'' -> ((j,) <$> lookupIssue m j key) <|> lookupI js'' key

data Issue = Issue
  { issueKey :: String
  , issueSummary :: String
  , issuePriority :: Maybe IssuePriority
  , issueAssignee :: Maybe User
  , issueStatus :: IssueStatus
  , issueReporter :: User
  } deriving Show

instance FromJSON Issue where
  parseJSON v = case v of
    Object o -> Issue
      <$> o .: "key"
      <*> o `field` "summary"
      <*> o `maybeField` "priority"
      <*> o `maybeField` "assignee"
      <*> o `field` "status"
      <*> o `field` "reporter"
    _ -> mzero

field :: FromJSON a => Object -> Text -> Parser a
field o n = o .: "fields" >>= (.: n)

maybeField :: FromJSON a => Object -> Text -> Parser (Maybe a)
maybeField o n = o .: "fields" >>= (.:? n)

data IssuePriority = IssuePriority
  { pname :: String
  , picon :: String
  } deriving Show

instance FromJSON IssuePriority where
  parseJSON v = case v of
    Object o -> IssuePriority <$> o .: "name" <*> o .: "iconUrl"
    _ -> mzero

newtype User = User
  { displayName :: String
  } deriving Show

instance FromJSON User where
  parseJSON v = case v of
    Object o -> User <$> o .: "displayName"
    _ -> mzero

data IssueStatus = IssueStatus
  { sname :: String
  } deriving Show

instance FromJSON IssueStatus where
  parseJSON v = case v of
    Object o -> IssueStatus
      <$> o .: "name"
    _ -> mzero

lookupIssue :: Manager -> Jira -> String -> MaybeT (Cmd (ModuleT S LB)) Issue
lookupIssue m j key = MaybeT $ catch lookup' handleErr where
  lookup' = liftIO $ do
    req <- newReq' j ("/rest/api/2/issue/" <> key)
    res <- httpLbs req m
    case eitherDecode $ responseBody res of
      Left e -> do
        putStr ("Failed parsing response from " ++ show (getUri req) ++ ": ")
        print e
        return Nothing
      Right i -> return i
  handleErr e = case e of
    StatusCodeException s _ _ | statusCode s /= 404 ->
      liftIO $ print e >> return Nothing
    _ -> return Nothing

sayLink' :: (Jira, Issue) -> Cmd (ModuleT S LB) ()
sayLink' (j, i) = getTarget >>= send' where
  send' = lift . lift . send . msg
  msg who = msg' { ircMsgParams = ircMsgParams msg' <> [html] } where
    msg' = privmsg who text
  text = concat
    [ "[", issueKey i, "] ", issueSummary i
    , "\n"
    , sname $ issueStatus i
    , maybe "" ((", " <>) . pname) (issuePriority i)
    , ", ", maybe "unassigned" (("assigned to " ++) . displayName) (issueAssignee i)
    , ", reported by ", displayName $ issueReporter i
    , "\n"
    , jiraUrl j, "browse/", issueKey i
    ]
  html = concat
    [ "[<a href=\"", jiraUrl j, "browse/", issueKey i, "\">", issueKey i, "</a>] "
    , maybe "" (<> " ") pimg
    , sname $ issueStatus i
    , ", ", maybe "unassigned" (("assigned to " ++) . displayName) (issueAssignee i)
    , ", reported by ", displayName $ issueReporter i
    , "<br>"
    , issueSummary i
    ] where
    pimg = flip fmap (issuePriority i) $ \p ->
      concat [ "<img src=\"", picon p, "\" title=\"", pname p, "\">" ]

newReq' :: Jira -> String -> IO Request
newReq' j p = newReq j p id

newReq :: Jira -> String -> (Request -> Request) -> IO Request
newReq j p f = f . addPath . auth <$> parseUrl (jiraUrl j) where
  auth = applyBasicAuth (pack $ jiraUser j) (pack $ jiraPass j)
  addPath req = req { path = path req <> pack p' } where
    p' | B.last (path req) == '/'
       , h:_ <- p
       , h == '/' = drop 1 p
       | otherwise = p

