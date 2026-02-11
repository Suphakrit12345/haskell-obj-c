{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BELayerHierarchyHandle@.
module ObjC.BrowserEngineKit.BELayerHierarchyHandle
  ( BELayerHierarchyHandle
  , IsBELayerHierarchyHandle(..)
  , init_
  , new
  , handleWithPort_data_error
  , encodeWithBlock
  , initSelector
  , newSelector
  , handleWithPort_data_errorSelector
  , encodeWithBlockSelector


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

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBELayerHierarchyHandle beLayerHierarchyHandle => beLayerHierarchyHandle -> IO (Id BELayerHierarchyHandle)
init_ beLayerHierarchyHandle  =
    sendMsg beLayerHierarchyHandle (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BELayerHierarchyHandle)
new  =
  do
    cls' <- getRequiredClass "BELayerHierarchyHandle"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Decodes a handle form a @mach_port_t@ send right and its accompanying metadata. - This method takes ownership of the port right (even if it returns an error).
--
-- ObjC selector: @+ handleWithPort:data:error:@
handleWithPort_data_error :: (IsNSData data_, IsNSError error_) => CUInt -> data_ -> error_ -> IO (Id BELayerHierarchyHandle)
handleWithPort_data_error port data_ error_ =
  do
    cls' <- getRequiredClass "BELayerHierarchyHandle"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "handleWithPort:data:error:") (retPtr retVoid) [argCUInt port, argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Encodes the handle into a @mach_port_t@ send right and its accompanying metadata. - The block is responsible for disposing of @copiedPort@ - failure to manage its lifecycle will leak the port. Note that some functions (like ``handleWithPort:data:error:``) will assume control of the right for you. - @copiedPort@ will be @MACH_PORT_NULL@ if the ``BELayerHierarchy`` pointed to by the handle is already invalidated. - The port and data should ultimately be consumed together  by ``handleWithPort:data:error:``.
--
-- ObjC selector: @- encodeWithBlock:@
encodeWithBlock :: IsBELayerHierarchyHandle beLayerHierarchyHandle => beLayerHierarchyHandle -> Ptr () -> IO ()
encodeWithBlock beLayerHierarchyHandle  block =
    sendMsg beLayerHierarchyHandle (mkSelector "encodeWithBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @handleWithPort:data:error:@
handleWithPort_data_errorSelector :: Selector
handleWithPort_data_errorSelector = mkSelector "handleWithPort:data:error:"

-- | @Selector@ for @encodeWithBlock:@
encodeWithBlockSelector :: Selector
encodeWithBlockSelector = mkSelector "encodeWithBlock:"

