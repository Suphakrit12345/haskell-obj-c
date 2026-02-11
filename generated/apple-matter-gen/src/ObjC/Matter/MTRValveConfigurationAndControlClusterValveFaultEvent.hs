{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRValveConfigurationAndControlClusterValveFaultEvent@.
module ObjC.Matter.MTRValveConfigurationAndControlClusterValveFaultEvent
  ( MTRValveConfigurationAndControlClusterValveFaultEvent
  , IsMTRValveConfigurationAndControlClusterValveFaultEvent(..)
  , valveFault
  , setValveFault
  , valveFaultSelector
  , setValveFaultSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- valveFault@
valveFault :: IsMTRValveConfigurationAndControlClusterValveFaultEvent mtrValveConfigurationAndControlClusterValveFaultEvent => mtrValveConfigurationAndControlClusterValveFaultEvent -> IO (Id NSNumber)
valveFault mtrValveConfigurationAndControlClusterValveFaultEvent  =
    sendMsg mtrValveConfigurationAndControlClusterValveFaultEvent (mkSelector "valveFault") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValveFault:@
setValveFault :: (IsMTRValveConfigurationAndControlClusterValveFaultEvent mtrValveConfigurationAndControlClusterValveFaultEvent, IsNSNumber value) => mtrValveConfigurationAndControlClusterValveFaultEvent -> value -> IO ()
setValveFault mtrValveConfigurationAndControlClusterValveFaultEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrValveConfigurationAndControlClusterValveFaultEvent (mkSelector "setValveFault:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valveFault@
valveFaultSelector :: Selector
valveFaultSelector = mkSelector "valveFault"

-- | @Selector@ for @setValveFault:@
setValveFaultSelector :: Selector
setValveFaultSelector = mkSelector "setValveFault:"

