{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BELayerHierarchyHostingTransactionCoordinator@.
module ObjC.BrowserEngineKit.BELayerHierarchyHostingTransactionCoordinator
  ( BELayerHierarchyHostingTransactionCoordinator
  , IsBELayerHierarchyHostingTransactionCoordinator(..)
  , init_
  , new
  , coordinatorWithError
  , addLayerHierarchy
  , commit
  , coordinatorWithPort_data_error
  , encodeWithBlock
  , initSelector
  , newSelector
  , coordinatorWithErrorSelector
  , addLayerHierarchySelector
  , commitSelector
  , coordinatorWithPort_data_errorSelector
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
init_ :: IsBELayerHierarchyHostingTransactionCoordinator beLayerHierarchyHostingTransactionCoordinator => beLayerHierarchyHostingTransactionCoordinator -> IO (Id BELayerHierarchyHostingTransactionCoordinator)
init_ beLayerHierarchyHostingTransactionCoordinator  =
    sendMsg beLayerHierarchyHostingTransactionCoordinator (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BELayerHierarchyHostingTransactionCoordinator)
new  =
  do
    cls' <- getRequiredClass "BELayerHierarchyHostingTransactionCoordinator"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | may fail if a connection to the render server cannot be established
--
-- ObjC selector: @+ coordinatorWithError:@
coordinatorWithError :: IsNSError error_ => error_ -> IO (Id BELayerHierarchyHostingTransactionCoordinator)
coordinatorWithError error_ =
  do
    cls' <- getRequiredClass "BELayerHierarchyHostingTransactionCoordinator"
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "coordinatorWithError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | a signal to coordinate transactions involving @layerHierarchy@ from now until @commit@ is called
--
-- ObjC selector: @- addLayerHierarchy:@
addLayerHierarchy :: (IsBELayerHierarchyHostingTransactionCoordinator beLayerHierarchyHostingTransactionCoordinator, IsBELayerHierarchy layerHierarchy) => beLayerHierarchyHostingTransactionCoordinator -> layerHierarchy -> IO ()
addLayerHierarchy beLayerHierarchyHostingTransactionCoordinator  layerHierarchy =
  withObjCPtr layerHierarchy $ \raw_layerHierarchy ->
      sendMsg beLayerHierarchyHostingTransactionCoordinator (mkSelector "addLayerHierarchy:") retVoid [argPtr (castPtr raw_layerHierarchy :: Ptr ())]

-- | @commit@ must be called on _every_ instance and it must be the last call to each instance. note that it does not commit @CATransaction@s but rather commits the coordination of transactions in the render server. note that coordinators should have as constrained a lifespan as possible and will timeout if held open too long.
--
-- ObjC selector: @- commit@
commit :: IsBELayerHierarchyHostingTransactionCoordinator beLayerHierarchyHostingTransactionCoordinator => beLayerHierarchyHostingTransactionCoordinator -> IO ()
commit beLayerHierarchyHostingTransactionCoordinator  =
    sendMsg beLayerHierarchyHostingTransactionCoordinator (mkSelector "commit") retVoid []

-- | Decodes a coordinator form a @mach_port_t@ send right and its accompanying metadata. - This method takes ownership of the port right (even if it returns an error).
--
-- ObjC selector: @+ coordinatorWithPort:data:error:@
coordinatorWithPort_data_error :: (IsNSData data_, IsNSError error_) => CUInt -> data_ -> error_ -> IO (Id BELayerHierarchyHostingTransactionCoordinator)
coordinatorWithPort_data_error port data_ error_ =
  do
    cls' <- getRequiredClass "BELayerHierarchyHostingTransactionCoordinator"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "coordinatorWithPort:data:error:") (retPtr retVoid) [argCUInt port, argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Encodes the coordinator into a @mach_port_t@ send right and its accompanying metadata. - The block is responsible for disposing of @copiedPort@ - failure to manage its lifecycle will leak the port. Note that some functions (like ``coordinatorWithPort:data:error:``) will assume control of the right for you. - @copiedPort@ will be @MACH_PORT_NULL@ if the receiver is already invalidated. - The port and data should ultimately be consumed together and _only_ once by ``coordinatorWithPort:data:error:``.
--
-- ObjC selector: @- encodeWithBlock:@
encodeWithBlock :: IsBELayerHierarchyHostingTransactionCoordinator beLayerHierarchyHostingTransactionCoordinator => beLayerHierarchyHostingTransactionCoordinator -> Ptr () -> IO ()
encodeWithBlock beLayerHierarchyHostingTransactionCoordinator  block =
    sendMsg beLayerHierarchyHostingTransactionCoordinator (mkSelector "encodeWithBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @coordinatorWithError:@
coordinatorWithErrorSelector :: Selector
coordinatorWithErrorSelector = mkSelector "coordinatorWithError:"

-- | @Selector@ for @addLayerHierarchy:@
addLayerHierarchySelector :: Selector
addLayerHierarchySelector = mkSelector "addLayerHierarchy:"

-- | @Selector@ for @commit@
commitSelector :: Selector
commitSelector = mkSelector "commit"

-- | @Selector@ for @coordinatorWithPort:data:error:@
coordinatorWithPort_data_errorSelector :: Selector
coordinatorWithPort_data_errorSelector = mkSelector "coordinatorWithPort:data:error:"

-- | @Selector@ for @encodeWithBlock:@
encodeWithBlockSelector :: Selector
encodeWithBlockSelector = mkSelector "encodeWithBlock:"

