{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module HKD where

import Data.Functor.Classes
import Data.Int
import Data.Text
import GHC.Generics (Generic)

-- HKD - Higher Kinded Data

-- Types of higher kinds are not types in themselves.
-- They only become types after you provide type arguments to them.
-- Examples:
--
-- Maybe :: Type -> Type
-- Either :: Type -> Type -> Type
-- Either Int :: Type -> Type
--
-- HKD in the example below refers to the "f" type argument.
-- We're referring to the fact that the Record type doesn't have any field of type "f"
-- instead "f" is only used to influence the final type of fields.
-- It's being used as part of "Required" type family which has kind "Fields -> Type -> Type"

data Fields
    = Defaults -- ^ A partially-specified record.
    | Complete -- ^ A fully-specified record.


type family Required (f :: Fields) a where
    Required Defaults a = () -- When defining defaults, required fields are ().
    Required Complete a = a


-- | Permanently deletes an empty bucket.
data StorageBucketsDelete f = StorageBucketsDelete
    { bucket :: Required f Text -- ^ Name of a bucket.
    , ifMetagenerationMatch :: Maybe Int64 -- ^ If set, only deletes the bucket if its metageneration matches this value.
    , ifMetagenerationNotMatch :: Maybe Int64 -- ^ If set, only deletes the bucket if its metageneration does not match this value.
    , provisionalUserProject :: Maybe Text -- ^ The project to be billed for this request if the target bucket is requester-pays bucket.
    , userProject :: Maybe Text -- ^ The project to be billed for this request. Required for Requester Pays buckets.
    , prettyPrint :: Bool -- ^ Whether the response should be pretty-printed. Defaults to 'True'.
    }


newStorageBucketsDelete :: StorageBucketsDelete Defaults
newStorageBucketsDelete =
    StorageBucketsDelete
        { bucket = () -- This is () because type family "Required Defaults Text" evaluates to ()
        , ifMetagenerationMatch = Nothing
        , ifMetagenerationNotMatch = Nothing
        , provisionalUserProject = Nothing
        , userProject = Nothing
        , prettyPrint = True
        }

-- Using "Complete" forces us to provide all required fields
myDelete :: StorageBucketsDelete Complete
myDelete = newStorageBucketsDelete{bucket = "Something"}


-- Due to the use of HKD in the record, you end up needing standalone deriving
-- for stock instances:

deriving stock instance Eq (StorageBucketsDelete Defaults)
deriving stock instance Eq (StorageBucketsDelete Complete)
deriving stock instance Show (StorageBucketsDelete Defaults)
deriving stock instance Show (StorageBucketsDelete Complete)

-- deriving stock instance Generic (StorageBucketsDelete f)
