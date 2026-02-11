{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRFetchRequest@.
module ObjC.SensorKit.SRFetchRequest
  ( SRFetchRequest
  , IsSRFetchRequest(..)
  , from
  , setFrom
  , to
  , setTo
  , device
  , setDevice
  , fromSelector
  , setFromSelector
  , toSelector
  , setToSelector
  , deviceSelector
  , setDeviceSelector


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

-- | Fetch data starting after this time.
--
-- This value must be specified for a valid request to be performed. If it is not specified, this will result in a SRErrorInvalidRequest error in the -sensorReader:fetchingRequest:failedWithError: SRSensorReaderDelegate callback.
--
-- The time range for fetching will be exclusive of start time and inclusive of end time: (start, end] . An SRSensorReader can use this to continue fetching a stream of data based on the last sample timestamp they have read.
--
-- ObjC selector: @- from@
from :: IsSRFetchRequest srFetchRequest => srFetchRequest -> IO CDouble
from srFetchRequest  =
    sendMsg srFetchRequest (mkSelector "from") retCDouble []

-- | Fetch data starting after this time.
--
-- This value must be specified for a valid request to be performed. If it is not specified, this will result in a SRErrorInvalidRequest error in the -sensorReader:fetchingRequest:failedWithError: SRSensorReaderDelegate callback.
--
-- The time range for fetching will be exclusive of start time and inclusive of end time: (start, end] . An SRSensorReader can use this to continue fetching a stream of data based on the last sample timestamp they have read.
--
-- ObjC selector: @- setFrom:@
setFrom :: IsSRFetchRequest srFetchRequest => srFetchRequest -> CDouble -> IO ()
setFrom srFetchRequest  value =
    sendMsg srFetchRequest (mkSelector "setFrom:") retVoid [argCDouble value]

-- | Fetch data ending at this time.
--
-- This value must be specified for a valid request to be performed. If it is not specified, this will result in a SRErrorInvalidRequest error in the -sensorReader:fetchingRequest:failedWithError: SRSensorReaderDelegate callback.
--
-- ObjC selector: @- to@
to :: IsSRFetchRequest srFetchRequest => srFetchRequest -> IO CDouble
to srFetchRequest  =
    sendMsg srFetchRequest (mkSelector "to") retCDouble []

-- | Fetch data ending at this time.
--
-- This value must be specified for a valid request to be performed. If it is not specified, this will result in a SRErrorInvalidRequest error in the -sensorReader:fetchingRequest:failedWithError: SRSensorReaderDelegate callback.
--
-- ObjC selector: @- setTo:@
setTo :: IsSRFetchRequest srFetchRequest => srFetchRequest -> CDouble -> IO ()
setTo srFetchRequest  value =
    sendMsg srFetchRequest (mkSelector "setTo:") retVoid [argCDouble value]

-- | Fetch data generated on this device
--
-- If this is not specified, the current device will be used.
--
-- ObjC selector: @- device@
device :: IsSRFetchRequest srFetchRequest => srFetchRequest -> IO (Id SRDevice)
device srFetchRequest  =
    sendMsg srFetchRequest (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Fetch data generated on this device
--
-- If this is not specified, the current device will be used.
--
-- ObjC selector: @- setDevice:@
setDevice :: (IsSRFetchRequest srFetchRequest, IsSRDevice value) => srFetchRequest -> value -> IO ()
setDevice srFetchRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg srFetchRequest (mkSelector "setDevice:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @from@
fromSelector :: Selector
fromSelector = mkSelector "from"

-- | @Selector@ for @setFrom:@
setFromSelector :: Selector
setFromSelector = mkSelector "setFrom:"

-- | @Selector@ for @to@
toSelector :: Selector
toSelector = mkSelector "to"

-- | @Selector@ for @setTo:@
setToSelector :: Selector
setToSelector = mkSelector "setTo:"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @setDevice:@
setDeviceSelector :: Selector
setDeviceSelector = mkSelector "setDevice:"

