{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRElectrocardiogramSample@.
module ObjC.SensorKit.SRElectrocardiogramSample
  ( SRElectrocardiogramSample
  , IsSRElectrocardiogramSample(..)
  , init_
  , new
  , date
  , frequency
  , session
  , lead
  , data_
  , initSelector
  , newSelector
  , dateSelector
  , frequencySelector
  , sessionSelector
  , leadSelector
  , dataSelector

  -- * Enum types
  , SRElectrocardiogramLead(SRElectrocardiogramLead)
  , pattern SRElectrocardiogramLeadRightArmMinusLeftArm
  , pattern SRElectrocardiogramLeadLeftArmMinusRightArm

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
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRElectrocardiogramSample srElectrocardiogramSample => srElectrocardiogramSample -> IO (Id SRElectrocardiogramSample)
init_ srElectrocardiogramSample  =
    sendMsg srElectrocardiogramSample (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRElectrocardiogramSample)
new  =
  do
    cls' <- getRequiredClass "SRElectrocardiogramSample"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | date
--
-- Date of the start of the batch of ECG data
--
-- ObjC selector: @- date@
date :: IsSRElectrocardiogramSample srElectrocardiogramSample => srElectrocardiogramSample -> IO (Id NSDate)
date srElectrocardiogramSample  =
    sendMsg srElectrocardiogramSample (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | frequency
--
-- Frequency in hertz at which the ECG data was recorded
--
-- ObjC selector: @- frequency@
frequency :: IsSRElectrocardiogramSample srElectrocardiogramSample => srElectrocardiogramSample -> IO (Id NSMeasurement)
frequency srElectrocardiogramSample  =
    sendMsg srElectrocardiogramSample (mkSelector "frequency") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | session
--
-- The session to which this sample belongs
--
-- ObjC selector: @- session@
session :: IsSRElectrocardiogramSample srElectrocardiogramSample => srElectrocardiogramSample -> IO (Id SRElectrocardiogramSession)
session srElectrocardiogramSample  =
    sendMsg srElectrocardiogramSample (mkSelector "session") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lead@
lead :: IsSRElectrocardiogramSample srElectrocardiogramSample => srElectrocardiogramSample -> IO SRElectrocardiogramLead
lead srElectrocardiogramSample  =
    fmap (coerce :: CLong -> SRElectrocardiogramLead) $ sendMsg srElectrocardiogramSample (mkSelector "lead") retCLong []

-- | @- data@
data_ :: IsSRElectrocardiogramSample srElectrocardiogramSample => srElectrocardiogramSample -> IO (Id NSArray)
data_ srElectrocardiogramSample  =
    sendMsg srElectrocardiogramSample (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @frequency@
frequencySelector :: Selector
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @session@
sessionSelector :: Selector
sessionSelector = mkSelector "session"

-- | @Selector@ for @lead@
leadSelector :: Selector
leadSelector = mkSelector "lead"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

