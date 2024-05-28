{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Configuration.LedgerDB (
    LedgerDbSelectorFlag(..)
  , Gigabytes
  , toBytes
  , completeSelector
  , defaultLMDBLimits
  , selectorToArgs
  ) where

import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Args
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Args as V1
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB (LMDBLimits (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import           Ouroboros.Consensus.Util.Args

import           Data.Functor.Identity
import qualified Data.Aeson.Types as Aeson (FromJSON)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Last(..))
import           Data.SOP.Dict

import           System.FilePath
import           System.FS.API
import           System.FS.IO

-- | Choose the LedgerDB Backend
--
-- As of UTxO-HD, the LedgerDB now uses either an in-memory backend or LMDB to
-- keep track of differences in the UTxO set.
--
-- - 'InMemory': uses more memory than the minimum requirements but is somewhat
--   faster.
-- - 'LMDB': uses less memory but is somewhat slower.
--
-- See 'Ouroboros.Consnesus.Storage.LedgerDB.OnDisk.BackingStoreSelector'.
data LedgerDbSelectorFlag f =
    V1LMDB
       (f V1.FlushFrequency)
       (f V1.QueryBatchSize)
       (f FilePath) -- ^ The path for the lmdb database
       (Maybe Gigabytes) -- ^ A map size can be specified, this is the maximum
                          -- disk space the LMDB database can fill. If not
                          -- provided, the default of 16GB will be used.
  | V1InMemory
      (f V1.FlushFrequency)
      (f V1.QueryBatchSize)
  | V2InMemory

deriving instance (Eq (f V1.FlushFrequency), Eq (f V1.QueryBatchSize), Eq (f FilePath)) => Eq (LedgerDbSelectorFlag f)
deriving instance (Show (f V1.FlushFrequency), Show (f V1.QueryBatchSize), Show (f FilePath)) => Show (LedgerDbSelectorFlag f)

-- | A number of gigabytes.
newtype Gigabytes = Gigabytes Int
  deriving stock (Eq, Show)
  deriving newtype (Read, Aeson.FromJSON)

-- | Convert a number of Gigabytes to the equivalent number of bytes.
toBytes :: Gigabytes -> Int
toBytes (Gigabytes x) = x * 1024 * 1024 * 1024

-- | Recommended settings for the LMDB backing store.
--
-- === @'lmdbMapSize'@
-- The default @'LMDBLimits'@ uses an @'lmdbMapSize'@ of @1024 * 1024 * 1024 * 16@
-- bytes, or 16 Gigabytes. @'lmdbMapSize'@ sets the size of the memory map
-- that is used internally by the LMDB backing store, and is also the
-- maximum size of the on-disk database. 16 GB should be sufficient for the
-- medium term, i.e., it is sufficient until a more performant alternative to
-- the LMDB backing store is implemented, which will probably replace the LMDB
-- backing store altogether.
--
-- Note(jdral): It is recommended not to set the @'lmdbMapSize'@ to a value
-- that is much smaller than 16 GB through manual configuration: the node will
-- die with a fatal error as soon as the database size exceeds the
-- @'lmdbMapSize'@. If this fatal error were to occur, we would expect that
-- the node can continue normal operation if it is restarted with a higher
-- @'lmdbMapSize'@ configured. Nonetheless, this situation should be avoided.
--
-- === @'lmdbMaxDatabases'@
-- The @'lmdbMaxDatabases'@ is set to 10, which means that the LMDB backing
-- store will allow up @<= 10@ internal databases. We say /internal/
-- databases, since they are not exposed outside the backing store interface,
-- such that from the outside view there is just one /logical/ database.
-- Two of these internal databases are reserved for normal operation of the
-- backing store, while the remaining databases will be used to store ledger
-- tables. At the moment, there is at most one ledger table that will be
-- stored in an internal database: the UTxO. Nonetheless, we set
-- @'lmdbMaxDatabases'@ to @10@ in order to future-proof these limits.
--
-- === @'lmdbMaxReaders'@
-- The @'lmdbMaxReaders'@ limit sets the maximum number of threads that can
-- read from the LMDB database. Currently, there should only be a single reader
-- active. Again, we set @'lmdbMaxReaders'@ to @16@ in order to future-proof
-- these limits.
--
-- === References
-- For more information about LMDB limits, one should inspect:
-- * The @lmdb-simple@ and @haskell-lmdb@ forked repositories.
-- * The official LMDB API documentation at
--    <http://www.lmdb.tech/doc/group__mdb.html>.
defaultLMDBLimits :: LMDBLimits
defaultLMDBLimits = LMDBLimits {
    lmdbMapSize = 16 * 1024 * 1024 * 1024
  , lmdbMaxDatabases = 10
  , lmdbMaxReaders = 16
  }

completeSelector :: LedgerDbSelectorFlag Last -> FilePath -> LedgerDbSelectorFlag Identity
completeSelector V2InMemory _ = V2InMemory
completeSelector (V1InMemory a b) _ =
  V1InMemory (Identity $ fromMaybe V1.DefaultFlushFrequency $ getLast a)
             (Identity $ fromMaybe V1.DefaultQueryBatchSize $ getLast b)
completeSelector (V1LMDB a b p l) db =
  V1LMDB (Identity $ fromMaybe V1.DefaultFlushFrequency $ getLast a)
         (Identity $ fromMaybe V1.DefaultQueryBatchSize $ getLast b)
         (Identity $ fromMaybe (db </> "lmdb") $ getLast p)
         l

selectorToArgs :: LedgerDbSelectorFlag Identity -> Complete LedgerDbFlavorArgs IO
selectorToArgs (V1InMemory (Identity a) (Identity b)) = LedgerDbFlavorArgsV1 $ V1.V1Args a b V1.InMemoryBackingStoreArgs
selectorToArgs V2InMemory = LedgerDbFlavorArgsV2 $ V2.V2Args V2.InMemoryHandleArgs
selectorToArgs (V1LMDB (Identity a) (Identity b) (Identity p) l) =
    LedgerDbFlavorArgsV1
  $ V1.V1Args a b
  $ V1.LMDBBackingStoreArgs
     (LiveLMDBFS $ SomeHasFS $ ioHasFS (MountPoint p))
     (maybe id (\ll lim -> lim { lmdbMapSize = toBytes ll }) l defaultLMDBLimits)
     Dict
