{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterNOCStruct@.
module ObjC.Matter.MTROperationalCredentialsClusterNOCStruct
  ( MTROperationalCredentialsClusterNOCStruct
  , IsMTROperationalCredentialsClusterNOCStruct(..)
  , noc
  , setNoc
  , icac
  , setIcac
  , vvsc
  , setVvsc
  , fabricIndex
  , setFabricIndex
  , nocSelector
  , setNocSelector
  , icacSelector
  , setIcacSelector
  , vvscSelector
  , setVvscSelector
  , fabricIndexSelector
  , setFabricIndexSelector


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

-- | @- noc@
noc :: IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct => mtrOperationalCredentialsClusterNOCStruct -> IO (Id NSData)
noc mtrOperationalCredentialsClusterNOCStruct  =
    sendMsg mtrOperationalCredentialsClusterNOCStruct (mkSelector "noc") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNoc:@
setNoc :: (IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct, IsNSData value) => mtrOperationalCredentialsClusterNOCStruct -> value -> IO ()
setNoc mtrOperationalCredentialsClusterNOCStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterNOCStruct (mkSelector "setNoc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- icac@
icac :: IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct => mtrOperationalCredentialsClusterNOCStruct -> IO (Id NSData)
icac mtrOperationalCredentialsClusterNOCStruct  =
    sendMsg mtrOperationalCredentialsClusterNOCStruct (mkSelector "icac") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIcac:@
setIcac :: (IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct, IsNSData value) => mtrOperationalCredentialsClusterNOCStruct -> value -> IO ()
setIcac mtrOperationalCredentialsClusterNOCStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterNOCStruct (mkSelector "setIcac:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vvsc@
vvsc :: IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct => mtrOperationalCredentialsClusterNOCStruct -> IO (Id NSData)
vvsc mtrOperationalCredentialsClusterNOCStruct  =
    sendMsg mtrOperationalCredentialsClusterNOCStruct (mkSelector "vvsc") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVvsc:@
setVvsc :: (IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct, IsNSData value) => mtrOperationalCredentialsClusterNOCStruct -> value -> IO ()
setVvsc mtrOperationalCredentialsClusterNOCStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterNOCStruct (mkSelector "setVvsc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct => mtrOperationalCredentialsClusterNOCStruct -> IO (Id NSNumber)
fabricIndex mtrOperationalCredentialsClusterNOCStruct  =
    sendMsg mtrOperationalCredentialsClusterNOCStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct, IsNSNumber value) => mtrOperationalCredentialsClusterNOCStruct -> value -> IO ()
setFabricIndex mtrOperationalCredentialsClusterNOCStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterNOCStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @noc@
nocSelector :: Selector
nocSelector = mkSelector "noc"

-- | @Selector@ for @setNoc:@
setNocSelector :: Selector
setNocSelector = mkSelector "setNoc:"

-- | @Selector@ for @icac@
icacSelector :: Selector
icacSelector = mkSelector "icac"

-- | @Selector@ for @setIcac:@
setIcacSelector :: Selector
setIcacSelector = mkSelector "setIcac:"

-- | @Selector@ for @vvsc@
vvscSelector :: Selector
vvscSelector = mkSelector "vvsc"

-- | @Selector@ for @setVvsc:@
setVvscSelector :: Selector
setVvscSelector = mkSelector "setVvsc:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

