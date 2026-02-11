{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRApplicationLauncherClusterApplicationStruct@.
module ObjC.Matter.MTRApplicationLauncherClusterApplicationStruct
  ( MTRApplicationLauncherClusterApplicationStruct
  , IsMTRApplicationLauncherClusterApplicationStruct(..)
  , catalogVendorID
  , setCatalogVendorID
  , catalogVendorId
  , setCatalogVendorId
  , applicationID
  , setApplicationID
  , applicationId
  , setApplicationId
  , catalogVendorIDSelector
  , setCatalogVendorIDSelector
  , catalogVendorIdSelector
  , setCatalogVendorIdSelector
  , applicationIDSelector
  , setApplicationIDSelector
  , applicationIdSelector
  , setApplicationIdSelector


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

-- | @- catalogVendorID@
catalogVendorID :: IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct => mtrApplicationLauncherClusterApplicationStruct -> IO (Id NSNumber)
catalogVendorID mtrApplicationLauncherClusterApplicationStruct  =
    sendMsg mtrApplicationLauncherClusterApplicationStruct (mkSelector "catalogVendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCatalogVendorID:@
setCatalogVendorID :: (IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct, IsNSNumber value) => mtrApplicationLauncherClusterApplicationStruct -> value -> IO ()
setCatalogVendorID mtrApplicationLauncherClusterApplicationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrApplicationLauncherClusterApplicationStruct (mkSelector "setCatalogVendorID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- catalogVendorId@
catalogVendorId :: IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct => mtrApplicationLauncherClusterApplicationStruct -> IO (Id NSNumber)
catalogVendorId mtrApplicationLauncherClusterApplicationStruct  =
    sendMsg mtrApplicationLauncherClusterApplicationStruct (mkSelector "catalogVendorId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCatalogVendorId:@
setCatalogVendorId :: (IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct, IsNSNumber value) => mtrApplicationLauncherClusterApplicationStruct -> value -> IO ()
setCatalogVendorId mtrApplicationLauncherClusterApplicationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrApplicationLauncherClusterApplicationStruct (mkSelector "setCatalogVendorId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- applicationID@
applicationID :: IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct => mtrApplicationLauncherClusterApplicationStruct -> IO (Id NSString)
applicationID mtrApplicationLauncherClusterApplicationStruct  =
    sendMsg mtrApplicationLauncherClusterApplicationStruct (mkSelector "applicationID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setApplicationID:@
setApplicationID :: (IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct, IsNSString value) => mtrApplicationLauncherClusterApplicationStruct -> value -> IO ()
setApplicationID mtrApplicationLauncherClusterApplicationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrApplicationLauncherClusterApplicationStruct (mkSelector "setApplicationID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- applicationId@
applicationId :: IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct => mtrApplicationLauncherClusterApplicationStruct -> IO (Id NSString)
applicationId mtrApplicationLauncherClusterApplicationStruct  =
    sendMsg mtrApplicationLauncherClusterApplicationStruct (mkSelector "applicationId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setApplicationId:@
setApplicationId :: (IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct, IsNSString value) => mtrApplicationLauncherClusterApplicationStruct -> value -> IO ()
setApplicationId mtrApplicationLauncherClusterApplicationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrApplicationLauncherClusterApplicationStruct (mkSelector "setApplicationId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @catalogVendorID@
catalogVendorIDSelector :: Selector
catalogVendorIDSelector = mkSelector "catalogVendorID"

-- | @Selector@ for @setCatalogVendorID:@
setCatalogVendorIDSelector :: Selector
setCatalogVendorIDSelector = mkSelector "setCatalogVendorID:"

-- | @Selector@ for @catalogVendorId@
catalogVendorIdSelector :: Selector
catalogVendorIdSelector = mkSelector "catalogVendorId"

-- | @Selector@ for @setCatalogVendorId:@
setCatalogVendorIdSelector :: Selector
setCatalogVendorIdSelector = mkSelector "setCatalogVendorId:"

-- | @Selector@ for @applicationID@
applicationIDSelector :: Selector
applicationIDSelector = mkSelector "applicationID"

-- | @Selector@ for @setApplicationID:@
setApplicationIDSelector :: Selector
setApplicationIDSelector = mkSelector "setApplicationID:"

-- | @Selector@ for @applicationId@
applicationIdSelector :: Selector
applicationIdSelector = mkSelector "applicationId"

-- | @Selector@ for @setApplicationId:@
setApplicationIdSelector :: Selector
setApplicationIdSelector = mkSelector "setApplicationId:"

