{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaInputClusterRenameInputParams@.
module ObjC.Matter.MTRMediaInputClusterRenameInputParams
  ( MTRMediaInputClusterRenameInputParams
  , IsMTRMediaInputClusterRenameInputParams(..)
  , index
  , setIndex
  , name
  , setName
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , indexSelector
  , setIndexSelector
  , nameSelector
  , setNameSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector


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

-- | @- index@
index :: IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams => mtrMediaInputClusterRenameInputParams -> IO (Id NSNumber)
index mtrMediaInputClusterRenameInputParams  =
    sendMsg mtrMediaInputClusterRenameInputParams (mkSelector "index") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIndex:@
setIndex :: (IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams, IsNSNumber value) => mtrMediaInputClusterRenameInputParams -> value -> IO ()
setIndex mtrMediaInputClusterRenameInputParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaInputClusterRenameInputParams (mkSelector "setIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams => mtrMediaInputClusterRenameInputParams -> IO (Id NSString)
name mtrMediaInputClusterRenameInputParams  =
    sendMsg mtrMediaInputClusterRenameInputParams (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams, IsNSString value) => mtrMediaInputClusterRenameInputParams -> value -> IO ()
setName mtrMediaInputClusterRenameInputParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaInputClusterRenameInputParams (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams => mtrMediaInputClusterRenameInputParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrMediaInputClusterRenameInputParams  =
    sendMsg mtrMediaInputClusterRenameInputParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams, IsNSNumber value) => mtrMediaInputClusterRenameInputParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrMediaInputClusterRenameInputParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaInputClusterRenameInputParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams => mtrMediaInputClusterRenameInputParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrMediaInputClusterRenameInputParams  =
    sendMsg mtrMediaInputClusterRenameInputParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams, IsNSNumber value) => mtrMediaInputClusterRenameInputParams -> value -> IO ()
setServerSideProcessingTimeout mtrMediaInputClusterRenameInputParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaInputClusterRenameInputParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @index@
indexSelector :: Selector
indexSelector = mkSelector "index"

-- | @Selector@ for @setIndex:@
setIndexSelector :: Selector
setIndexSelector = mkSelector "setIndex:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

