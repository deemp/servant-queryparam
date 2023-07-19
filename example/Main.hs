{-
## Example

This example demonstrates servant APIs with query parameters.
There's an OpenAPI specification for each of them.

First, I enable compiler extensions.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-
Next, I import the necessary modules.
-}
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson (ToJSON)
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Data (Proxy (..), Typeable)
import Data.Maybe (maybeToList)
import Data.OpenApi (OpenApi, ToParamSchema, ToSchema)
import Data.OpenApi.Internal.Utils (encodePretty)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Application, Context (..), GenericMode ((:-)), Get, Handler (..), JSON, NamedRoutes, ServerError, (:>))
import Servant.OpenApi (HasOpenApi (..))
import Servant.QueryParam.OpenApi.Record ()
import Servant.QueryParam.Record (RecordParam)
import Servant.QueryParam.TypeLevel (DropPrefix, Eval, Exp)
import Servant.Server.Generic (genericServe, genericServeTWithContext)
import Servant.QueryParam.Server.Record ()

{-
I define a label for dropping the prefix of a `Symbol`.
-}
data DropPrefixExp :: Symbol -> Exp Symbol

{-
Next, I provide an interpreter for that wrapper.
-}
type instance Eval (DropPrefixExp sym) = DropPrefix sym

{-
`DropPrefix` drops the leading `'_'` characters, then the non-`'_'` characters, then the `'_'` characters.
-}

-- >>> :kind! DropPrefix "_params_user"
-- DropPrefix "_params_user" :: Symbol
-- = "user"

{-
Then, I define a label for keeping the prefix of a `Symbol`.
-}
data KeepPrefixExp :: Symbol -> Exp Symbol

{-
The interpreter of this label does nothing to a `Symbol`.
-}
type instance Eval (KeepPrefixExp sym) = sym

{-
Now, I write a record.
I'll use this record to produce query parameters.
-}
data Params = Params
  { _params_user :: Maybe String
  , _params_users :: [String]
  , _params_oneUser :: String
  , _params_userFlag :: Bool
  }
  deriving (Show, Generic, Typeable, ToJSON, ToSchema)

{-
This is our first API.
In this API, the query parameter names are the record field names with dropped prefixes.
E.g., the query parameter `user` corresponds to the field name `_params_user`.
-}
newtype UserAPI1 routes = UserAPI1
  { get :: routes :- "get" :> RecordParam DropPrefixExp Params :> Get '[JSON] [String]
  }
  deriving (Generic)

{-
This is our second API.
In this API, query parameter names are the record field names.
E.g., the query parameter `_params_user` corresponds to the field name `_params_user`.
-}
newtype UserAPI2 routes = UserAPI2
  { get :: routes :- "get" :> RecordParam KeepPrefixExp Params :> Get '[JSON] [String]
  }
  deriving (Generic)

{-
Now, I define an OpenAPI specification for the first API.
-}
specDrop :: OpenApi
specDrop = toOpenApi (Proxy :: Proxy (NamedRoutes UserAPI1))

{-
Then, I define an OpenAPI specification for the second API.
-}
specKeep :: OpenApi
specKeep = toOpenApi (Proxy :: Proxy (NamedRoutes UserAPI2))

{-
Following that, I define an `Application`.
At the `/get` endpoint, the application returns a list of query parameter values.
-}
server :: Application
server =
  genericServeTWithContext
    (\x -> Servant.Handler (ExceptT $ Right <$> liftIO x))
    UserAPI1
      { get = \Params{..} ->
          pure $
            maybeToList _params_user
              <> _params_users
              <> [_params_oneUser, show _params_userFlag]
      }
    EmptyContext

{-
Finally, I combine all parts.

First, the application will print the OpenAPI specifications in JSON format. Next, it will start a server.
-}
main :: IO ()
main = do
  putStrLn "\n---\nQuery parameters without prefixes\n---\n"
  BSL8.putStrLn $ encodePretty specDrop
  putStrLn "\n---\nQuery parameters with prefixes\n---\n"
  BSL8.putStrLn $ encodePretty specKeep

  putStrLn "\n---\nStarting the server...\n---\n"
  putStrLn "\nTry running\n"
  putStrLn "curl -v \"localhost:8080/get?user=1&users=2&users=3&oneUser=4&userFlag=true\""
  Warp.run 8080 server

{-
## Run

Run the server.

```hs
cabal run example
```

## Test

When the server is up, request the `/get` endpoint via `curl`.

```console
curl -v "localhost:8080/get?user=1&users=2&users=3&oneUser=4&userFlag=true"
```

You should receive the values of query parameters in a list.

```console
...
["1","2","3","4","True"]
```
-}
