{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
import Servant.OpenApi.Record ()
import Servant.Record (RecordParam)
import Servant.Server.Generic (genericServe, genericServeTWithContext)
import Servant.Server.Record ()
import Servant.TypeLevel ( Eval, Exp, DropPrefix )

-- | A label for dropping the prefix of a 'Symbol'
data DropPrefixExp :: Symbol -> Exp Symbol

type instance Eval (DropPrefixExp sym) = DropPrefix sym

-- | A label for keeping the prefix of a 'Symbol'
data KeepPrefixExp :: Symbol -> Exp Symbol

type instance Eval (KeepPrefixExp sym) = sym

-- | Query parameters as a record
data Params = Params
  { _get_user :: Maybe String,
    _get_users :: [String],
    _get_oneUser :: String,
    _get_userFlag :: Bool
  }
  deriving (Show, Generic, Typeable, ToJSON, ToSchema)

-- | User id
newtype UserId = UserId Integer
  deriving (Show, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema, ToParamSchema)

-- | API as a record. Prefixes of query parameters are dropped
newtype UserAPI1 routes = UserAPI1 {get :: routes :- "get" :> RecordParam DropPrefixExp Params :> Get '[JSON] [String]} deriving (Generic)

-- | API as a type synonym
type APIDrop = NamedRoutes UserAPI1

-- | API as a record. Prefixes of query parameters are kept
newtype UserAPI2 routes = UserAPI2 {get :: routes :- "get" :> RecordParam KeepPrefixExp Params :> Get '[JSON] [String]} deriving (Generic)

-- | API as a type synonym
type APIKeep = NamedRoutes UserAPI2

-- | 'OpenApi' specification for 'APIDrop'
specDrop :: OpenApi
specDrop = toOpenApi (Proxy :: Proxy APIDrop)

-- | 'OpenApi' specification for 'APIKeep'
specKeep :: OpenApi
specKeep = toOpenApi (Proxy :: Proxy APIKeep)

server :: Application
server =
  genericServeTWithContext
    (\x -> Servant.Handler (ExceptT $ Right <$> liftIO x))
    UserAPI1
      { get = \Params {..} -> pure $ maybeToList _get_user <> _get_users <> [_get_oneUser, show _get_userFlag]
      }
    EmptyContext

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