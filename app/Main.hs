{-# LANGUAGE OverloadedStrings #-}

import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified GHC.Generics as BU
import Network.HTTP.Types (ResponseHeaders, Status (statusCode, statusMessage), status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp
import Text.Printf (printf)

main = do
  let port = 3000
  putStrLn $ "Listening on http://localhost:" ++ show port
  run port $ midLog app

app :: Application
app req respond =
  respond $
    case pathInfo req of
      [] -> index
      ["yay"] -> yay
      ["htmx"] -> resJs "htmx.min.js"
      o -> responseBuilder status404 textPlain "big evil"

midLog :: Middleware
midLog app req res = do
  let method = BU.toString $ requestMethod req
  let path = BU.toString $ rawPathInfo req
  let res' h = do
        let status = responseStatus h
        let code = statusCode status
        let message = BU.toString $ statusMessage status
        printf "%d %s %s %s\n" code message method path
        res h
  app req res'

yay =
  responseBuilder
    status200
    textPlain
    $ mconcat [copyByteString "yay"]

index =
  responseBuilder status200 textHtml $
    mconcat $
      map
        copyByteString
        [ "<script src=\"/htmx\"></script>",
          "<div hx-swap=\"outerHTML\" hx-get=\"/yay\">Click me!</div>"
        ]

contentType :: BU.ByteString -> ResponseHeaders
contentType h = [("Content-Type", h)]

textPlain = contentType "text/plain"

textHtml = contentType "text/html"

textJs = contentType "text/javascript"

resFile mime filename = responseFile status200 mime filename Nothing

resHtml = resFile textHtml

resJs = resFile textJs
