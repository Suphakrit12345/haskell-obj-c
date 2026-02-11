{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestDifferentVendorMeiEventEvent@.
module ObjC.Matter.MTRUnitTestingClusterTestDifferentVendorMeiEventEvent
  ( MTRUnitTestingClusterTestDifferentVendorMeiEventEvent
  , IsMTRUnitTestingClusterTestDifferentVendorMeiEventEvent(..)
  , arg1
  , setArg1
  , arg1Selector
  , setArg1Selector


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

-- | @- arg1@
arg1 :: IsMTRUnitTestingClusterTestDifferentVendorMeiEventEvent mtrUnitTestingClusterTestDifferentVendorMeiEventEvent => mtrUnitTestingClusterTestDifferentVendorMeiEventEvent -> IO (Id NSNumber)
arg1 mtrUnitTestingClusterTestDifferentVendorMeiEventEvent  =
    sendMsg mtrUnitTestingClusterTestDifferentVendorMeiEventEvent (mkSelector "arg1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg1:@
setArg1 :: (IsMTRUnitTestingClusterTestDifferentVendorMeiEventEvent mtrUnitTestingClusterTestDifferentVendorMeiEventEvent, IsNSNumber value) => mtrUnitTestingClusterTestDifferentVendorMeiEventEvent -> value -> IO ()
setArg1 mtrUnitTestingClusterTestDifferentVendorMeiEventEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestDifferentVendorMeiEventEvent (mkSelector "setArg1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arg1@
arg1Selector :: Selector
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector
setArg1Selector = mkSelector "setArg1:"

