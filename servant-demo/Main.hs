{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Data (Proxy (..))
import Network.Wai.Handler.Warp (run)
import Servant (Handler)
import Servant.API (Capture, Get, JSON, type (:>))
import Servant.Server (Application, Server, serve)


{-
import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    extension <- parseBody "asdf"
    html $ mconcat ["Scotty, ", beam, " me up!"]
-}

type HelloAPI = Capture "word" String :> Get '[JSON] String


-- λ> :kind! MyServer
-- MyServer :: *
-- = Int -> Handler String
type MyServer = Server HelloAPI


myHandler :: String -> Handler String
myHandler word = pure $ "Scotty " <> word <> " me up!"


main :: IO ()
main =
    run 3000 application
  where
    application :: Application
    application = serve (Proxy :: Proxy HelloAPI) myHandler

{-
After running this (e.g. stack run on CLI or execute `main` in GHCi)
run the following in terminal:

$ curl -v http://localhost:3000/beam -H 'Accept: application/json'
*   Trying ::1:3000...
* connect to ::1 port 3000 failed: Connection refused
*   Trying 127.0.0.1:3000...
* Connected to localhost (127.0.0.1) port 3000 (#0)
> GET /beam HTTP/1.1
> Host: localhost:3000
> User-Agent: curl/7.69.1
> Accept: application/json
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Transfer-Encoding: chunked
< Date: Thu, 08 Oct 2020 04:36:08 GMT
< Server: Warp/3.3.13
< Content-Type: application/json;charset=utf-8
<
* Connection #0 to host localhost left intact
"Scotty beam me up!"
-}
