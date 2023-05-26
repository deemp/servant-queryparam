{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports orphan instances to make
-- [@servant-openapi3@](https://hackage.haskell.org/package/servant-openapi3) work with [@servant-queryparam-core@](https://hackage.haskell.org/package/servant-queryparam-core).
module Servant.QueryParam.OpenApi.Record () where

import Data.OpenApi
import Data.Proxy
import Servant.API
import Servant.OpenApi
import Servant.QueryParam.Record

instance HasOpenApi (UnRecordParam mod (RecordParam mod a :> api)) => HasOpenApi (RecordParam mod a :> api) where
  toOpenApi :: Proxy (RecordParam mod a :> api) -> OpenApi
  toOpenApi _ = toOpenApi (Proxy :: Proxy (UnRecordParam mod (RecordParam mod a :> api)))
