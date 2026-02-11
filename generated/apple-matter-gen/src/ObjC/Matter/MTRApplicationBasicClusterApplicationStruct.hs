{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRApplicationBasicClusterApplicationStruct@.
module ObjC.Matter.MTRApplicationBasicClusterApplicationStruct
  ( MTRApplicationBasicClusterApplicationStruct
  , IsMTRApplicationBasicClusterApplicationStruct(..)
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
catalogVendorID :: IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct => mtrApplicationBasicClusterApplicationStruct -> IO (Id NSNumber)
catalogVendorID mtrApplicationBasicClusterApplicationStruct  =
    sendMsg mtrApplicationBasicClusterApplicationStruct (mkSelector "catalogVendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCatalogVendorID:@
setCatalogVendorID :: (IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct, IsNSNumber value) => mtrApplicationBasicClusterApplicationStruct -> value -> IO ()
setCatalogVendorID mtrApplicationBasicClusterApplicationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrApplicationBasicClusterApplicationStruct (mkSelector "setCatalogVendorID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- catalogVendorId@
catalogVendorId :: IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct => mtrApplicationBasicClusterApplicationStruct -> IO (Id NSNumber)
catalogVendorId mtrApplicationBasicClusterApplicationStruct  =
    sendMsg mtrApplicationBasicClusterApplicationStruct (mkSelector "catalogVendorId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCatalogVendorId:@
setCatalogVendorId :: (IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct, IsNSNumber value) => mtrApplicationBasicClusterApplicationStruct -> value -> IO ()
setCatalogVendorId mtrApplicationBasicClusterApplicationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrApplicationBasicClusterApplicationStruct (mkSelector "setCatalogVendorId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- applicationID@
applicationID :: IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct => mtrApplicationBasicClusterApplicationStruct -> IO (Id NSString)
applicationID mtrApplicationBasicClusterApplicationStruct  =
    sendMsg mtrApplicationBasicClusterApplicationStruct (mkSelector "applicationID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setApplicationID:@
setApplicationID :: (IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct, IsNSString value) => mtrApplicationBasicClusterApplicationStruct -> value -> IO ()
setApplicationID mtrApplicationBasicClusterApplicationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrApplicationBasicClusterApplicationStruct (mkSelector "setApplicationID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- applicationId@
applicationId :: IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct => mtrApplicationBasicClusterApplicationStruct -> IO (Id NSString)
applicationId mtrApplicationBasicClusterApplicationStruct  =
    sendMsg mtrApplicationBasicClusterApplicationStruct (mkSelector "applicationId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setApplicationId:@
setApplicationId :: (IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct, IsNSString value) => mtrApplicationBasicClusterApplicationStruct -> value -> IO ()
setApplicationId mtrApplicationBasicClusterApplicationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrApplicationBasicClusterApplicationStruct (mkSelector "setApplicationId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

