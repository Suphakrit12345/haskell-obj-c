{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKHeartbeatSeriesQuery@.
module ObjC.HealthKit.HKHeartbeatSeriesQuery
  ( HKHeartbeatSeriesQuery
  , IsHKHeartbeatSeriesQuery(..)
  , initWithHeartbeatSeries_dataHandler
  , initWithHeartbeatSeries_dataHandlerSelector


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

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithHeartbeatSeries:dataHandler:
--
-- Returns a query that will retrieve heartbeat timestamps for the specified HKHeartbeatSeriesSample.
--
-- @heartbeatSeries@ — The HKHeartbeatSeriesSample for which the heartbeat data will be returned.
--
-- @dataHandler@ — The block to invoke with results from the query. It is called repeatedly for each                                   heartbeat in the series. timeSinceSeriesStart is the time elapsed in seconds after the                                   series startDate that represents when the heartbeat occured. precededByGap indicates if                                   there was a gap in data collection before the current heartbeat, meaning that one or more                                   heartbeats may have occured since the previous heartbeat in the series. Once done is YES,                                   or stopQuery called, the query is complete and no more calls to the handler will be made.
--
-- ObjC selector: @- initWithHeartbeatSeries:dataHandler:@
initWithHeartbeatSeries_dataHandler :: (IsHKHeartbeatSeriesQuery hkHeartbeatSeriesQuery, IsHKHeartbeatSeriesSample heartbeatSeries) => hkHeartbeatSeriesQuery -> heartbeatSeries -> Ptr () -> IO (Id HKHeartbeatSeriesQuery)
initWithHeartbeatSeries_dataHandler hkHeartbeatSeriesQuery  heartbeatSeries dataHandler =
  withObjCPtr heartbeatSeries $ \raw_heartbeatSeries ->
      sendMsg hkHeartbeatSeriesQuery (mkSelector "initWithHeartbeatSeries:dataHandler:") (retPtr retVoid) [argPtr (castPtr raw_heartbeatSeries :: Ptr ()), argPtr (castPtr dataHandler :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHeartbeatSeries:dataHandler:@
initWithHeartbeatSeries_dataHandlerSelector :: Selector
initWithHeartbeatSeries_dataHandlerSelector = mkSelector "initWithHeartbeatSeries:dataHandler:"

