{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRFetchResult@.
module ObjC.SensorKit.SRFetchResult
  ( SRFetchResult
  , IsSRFetchResult(..)
  , init_
  , new
  , sample
  , timestamp
  , initSelector
  , newSelector
  , sampleSelector
  , timestampSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRFetchResult srFetchResult => srFetchResult -> IO (Id SRFetchResult)
init_ srFetchResult  =
    sendMsg srFetchResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRFetchResult)
new  =
  do
    cls' <- getRequiredClass "SRFetchResult"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Retrieves the resultant sample
--
-- The caller is expected to know what the result type should be
--
-- Note: This may thrown an exception if the sample could not be constructed from the data in the datastore
--
-- ObjC selector: @- sample@
sample :: IsSRFetchResult srFetchResult => srFetchResult -> IO RawId
sample srFetchResult  =
    fmap (RawId . castPtr) $ sendMsg srFetchResult (mkSelector "sample") (retPtr retVoid) []

-- | the timestamp the sample was written to the data store
--
-- ObjC selector: @- timestamp@
timestamp :: IsSRFetchResult srFetchResult => srFetchResult -> IO CDouble
timestamp srFetchResult  =
    sendMsg srFetchResult (mkSelector "timestamp") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sample@
sampleSelector :: Selector
sampleSelector = mkSelector "sample"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

