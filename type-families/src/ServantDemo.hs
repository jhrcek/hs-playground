{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ServantDemo where

import Data.Data (Proxy (..))
import Servant.API
import Servant.Links
import Servant.Server


type AddEndpoint = "add" :> Capture "a" Int :> Capture "b" Int :> Get '[JSON] Int


type IsEvenEndpoint = "is-even" :> Capture "x" Int :> Get '[JSON] Bool


type MyApi = AddEndpoint :<|> IsEvenEndpoint


-- Our API types are used as inputs to bunch of type classes and type families.
-- HasServer type class has associated ServerT type family which computes the types of handlers
-- that we have to implement
-- >>> :kind! ServerT AddEndpoint Handler
-- ServerT AddEndpoint Handler :: *
-- = Int -> Int -> Handler Int
--

-- Another example - generating safe links which are guaranteed to exist within our API
-- HasLink type class has MkLink type family which computes the type of "link generating function"
-- >>> :kind! MkLink AddEndpoint Link
-- MkLink AddEndpoint Link :: *
-- = Int -> Int -> Link
addEndpointLink :: Int -> Int -> URI
addEndpointLink a b = linkURI $ safeLink (Proxy :: Proxy MyApi) (Proxy :: Proxy AddEndpoint) a b
