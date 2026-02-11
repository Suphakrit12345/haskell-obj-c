{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKElectrocardiogramQuery@.
module ObjC.HealthKit.HKElectrocardiogramQuery
  ( HKElectrocardiogramQuery
  , IsHKElectrocardiogramQuery(..)
  , initWithElectrocardiogram_dataHandler
  , initWithElectrocardiogram_dataHandlerSelector


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

-- | initWithElectrocardiogram:dataHandler:
--
-- Returns a query that will enumerate over voltages recorded across leads in               an electrocardiogram.
--
-- @electrocardiogram@ — The sample for which the lead data will be returned.
--
-- @dataHandler@ — The block to invoke with results from the query. It will be called once for each voltage measurement. Call [query stop] to stop enumeration, if desired.
--
-- ObjC selector: @- initWithElectrocardiogram:dataHandler:@
initWithElectrocardiogram_dataHandler :: (IsHKElectrocardiogramQuery hkElectrocardiogramQuery, IsHKElectrocardiogram electrocardiogram) => hkElectrocardiogramQuery -> electrocardiogram -> Ptr () -> IO (Id HKElectrocardiogramQuery)
initWithElectrocardiogram_dataHandler hkElectrocardiogramQuery  electrocardiogram dataHandler =
  withObjCPtr electrocardiogram $ \raw_electrocardiogram ->
      sendMsg hkElectrocardiogramQuery (mkSelector "initWithElectrocardiogram:dataHandler:") (retPtr retVoid) [argPtr (castPtr raw_electrocardiogram :: Ptr ()), argPtr (castPtr dataHandler :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithElectrocardiogram:dataHandler:@
initWithElectrocardiogram_dataHandlerSelector :: Selector
initWithElectrocardiogram_dataHandlerSelector = mkSelector "initWithElectrocardiogram:dataHandler:"

