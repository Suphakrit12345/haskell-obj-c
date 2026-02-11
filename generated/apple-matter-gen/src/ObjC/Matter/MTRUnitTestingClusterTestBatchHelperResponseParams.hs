{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestBatchHelperResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestBatchHelperResponseParams
  ( MTRUnitTestingClusterTestBatchHelperResponseParams
  , IsMTRUnitTestingClusterTestBatchHelperResponseParams(..)
  , initWithResponseValue_error
  , buffer
  , setBuffer
  , initWithResponseValue_errorSelector
  , bufferSelector
  , setBufferSelector


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

-- | Initialize an MTRUnitTestingClusterTestBatchHelperResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestBatchHelperResponseParams mtrUnitTestingClusterTestBatchHelperResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestBatchHelperResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestBatchHelperResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestBatchHelperResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrUnitTestingClusterTestBatchHelperResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- buffer@
buffer :: IsMTRUnitTestingClusterTestBatchHelperResponseParams mtrUnitTestingClusterTestBatchHelperResponseParams => mtrUnitTestingClusterTestBatchHelperResponseParams -> IO (Id NSData)
buffer mtrUnitTestingClusterTestBatchHelperResponseParams  =
    sendMsg mtrUnitTestingClusterTestBatchHelperResponseParams (mkSelector "buffer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBuffer:@
setBuffer :: (IsMTRUnitTestingClusterTestBatchHelperResponseParams mtrUnitTestingClusterTestBatchHelperResponseParams, IsNSData value) => mtrUnitTestingClusterTestBatchHelperResponseParams -> value -> IO ()
setBuffer mtrUnitTestingClusterTestBatchHelperResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestBatchHelperResponseParams (mkSelector "setBuffer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @buffer@
bufferSelector :: Selector
bufferSelector = mkSelector "buffer"

-- | @Selector@ for @setBuffer:@
setBufferSelector :: Selector
setBufferSelector = mkSelector "setBuffer:"

