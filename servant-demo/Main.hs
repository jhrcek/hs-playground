{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Data (Proxy (..))
import Network.Wai.Handler.Warp (run)
import Servant (Handler)
import Servant.API (Capture, Get, JSON, type (:>))
import Servant.Server (Application, Server, serve)


type HelloAPI = "hello" :> Capture "name" String :> Get '[JSON] String


myHandler :: String -> Handler String
myHandler name = pure $ "Hello " <> name <> "!"


application :: Application
application = serve (Proxy :: Proxy HelloAPI) myHandler


main :: IO ()
main = do
    putStrLn "Listening on http://localhost:3000/hello/jan"
    run 3000 application


{-
After running this (e.g. stack run on CLI or execute `main` in GHCi)
run the following in terminal:

curl -v http://localhost:3000/hello/jan -H 'content-type: application/json'
*   Trying 127.0.0.1:3000...
* Connected to localhost (127.0.0.1) port 3000 (#0)
> GET /hello/jan HTTP/1.1
> Host: localhost:3000
> User-Agent: curl/7.79.1
> Accept: */*
> content-type: application/json
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Transfer-Encoding: chunked
< Date: Thu, 28 Jul 2022 07:06:32 GMT
< Server: Warp/3.3.18
< Content-Type: application/json;charset=utf-8
<
* Connection #0 to host localhost left intact
"Hello jan!"
-}

{-
import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    extension <- parseBody "asdf"
    html $ mconcat ["Scotty, ", beam, " me up!"]
-}

-- λ> :kind! MyServer
-- MyServer :: *
-- = Int -> Handler String
type MyServer = Server HelloAPI
