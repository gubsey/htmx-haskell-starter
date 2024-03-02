{-# LANGUAGE OverloadedStrings #-}

import Blaze.ByteString.Builder.Char8 (fromString)
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString.UTF8 as BU
import Data.Int (Int64)
import Data.Text (unpack)
import Data.Time
import Database.SQLite.Simple
import Network.HTTP.Types
  ( ResponseHeaders,
    Status (statusCode, statusMessage),
    status200,
    status404,
  )
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Parse (Param, lbsBackEnd, parseRequestBody)
import Text.Printf (printf)

main :: IO ()
main = do
  setupDB
  putStrLn $ "Listening on http://localhost:" ++ show port
  run port $ midLog app
  where
    port = 3000

app :: Application
app req respond = do
  (params, _) <- parseRequestBody lbsBackEnd req
  res <- case mepinf of
    ("DELETE", ["deltodo", did]) -> delTodo $ unpack did
    ("GET", ["todos"]) -> do resHtmlString . todosToUl <$> todoList
    ("POST", ["form"]) -> formPost params
    _nonIo -> return $ case mepinf of
      ("GET", []) -> resHtmlFile "app/form.html"
      ("GET", ["htmx"]) -> resJs "app/htmx.min.js"
      _undefinedPath -> errorPage $ rawPath ++ " is undefined" where rawPath = toString $ rawPathInfo req
  respond res
  where
    pinf = pathInfo req
    meth = requestMethod req
    mepinf = (meth, pinf)

midLog :: Middleware
midLog midApp req res = do
  start <- getCurrentTime
  let method = BU.toString $ requestMethod req
  let path = BU.toString $ rawPathInfo req
  let res' h = do
        end <- getCurrentTime
        let diff = show $ diffUTCTime start end
        let status = responseStatus h
        let code = statusCode status
        let message = BU.toString $ statusMessage status
        printf "%d %s %s %s\n" code method path diff
        res h
  midApp req res'

setupDB = do
  conn <- getConn
  execute_ conn "CREATE TABLE IF NOT EXISTS todos (todo VARCHAR(255))"

data TodoField = TodoField !Int64 !String deriving (Show)

instance FromRow TodoField where
  fromRow = TodoField <$> field <*> field

todoToLi :: TodoField -> String
todoToLi (TodoField i s) = printf "<li hx-target=\"this\"><button hx-swap=\"outerHTML\" hx-delete=\"/deltodo/%d\">x</button> | %s</li>" i s :: String

todosToUl :: [TodoField] -> String
todosToUl todos = "<ul id=\"todos\">" ++ h ++ "</ul>"
  where
    h = concatMap todoToLi todos

todoList :: IO [TodoField]
todoList = do
  conn <- getConn
  r <- query_ conn "SELECT rowid, todo FROM todos" :: IO [TodoField]
  close conn
  return r

delTodo :: String -> IO Response
delTodo did = do
  conn <- getConn
  execute conn "DELETE FROM todos WHERE rowid=?" (Only did)
  close conn
  return $ responseBuilder status200 textHtml $ fromString ""

formPost :: [Param] -> IO Response
formPost params =
  case params of
    [("msg", msg)] -> do
      conn <- getConn
      execute conn "INSERT INTO todos (todo) VALUES (?)" (Only smsg)
      rowId <- lastInsertRowId conn
      close conn
      return $ resHtmlString $ todoToLi $ TodoField rowId smsg
      where
        smsg = toString msg
    _badBody -> return $ errorPage "invalid post body"

getConn :: IO Connection
getConn = open "db.db"

errorPage :: String -> Response
errorPage e = responseBuilder status404 textPlain $ fromString e

contentType :: BU.ByteString -> ResponseHeaders
contentType h = [("Content-Type", h)]

textPlain :: ResponseHeaders
textPlain = contentType "text/plain"

textHtml :: ResponseHeaders
textHtml = contentType "text/html"

textJs :: ResponseHeaders
textJs = contentType "text/javascript"

resFile :: ResponseHeaders -> FilePath -> Response
resFile mime filename = responseFile status200 mime filename Nothing

resHtmlFile :: FilePath -> Response
resHtmlFile = resFile textHtml

resHtmlString :: String -> Response
resHtmlString s = responseBuilder status200 textHtml $ fromString s

resJs :: FilePath -> Response
resJs = resFile textJs
