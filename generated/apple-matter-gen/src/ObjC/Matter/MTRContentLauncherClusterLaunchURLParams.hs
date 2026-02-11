{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterLaunchURLParams@.
module ObjC.Matter.MTRContentLauncherClusterLaunchURLParams
  ( MTRContentLauncherClusterLaunchURLParams
  , IsMTRContentLauncherClusterLaunchURLParams(..)
  , contentURL
  , setContentURL
  , displayString
  , setDisplayString
  , brandingInformation
  , setBrandingInformation
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , contentURLSelector
  , setContentURLSelector
  , displayStringSelector
  , setDisplayStringSelector
  , brandingInformationSelector
  , setBrandingInformationSelector
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

-- | @- contentURL@
contentURL :: IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams => mtrContentLauncherClusterLaunchURLParams -> IO (Id NSString)
contentURL mtrContentLauncherClusterLaunchURLParams  =
    sendMsg mtrContentLauncherClusterLaunchURLParams (mkSelector "contentURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentURL:@
setContentURL :: (IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams, IsNSString value) => mtrContentLauncherClusterLaunchURLParams -> value -> IO ()
setContentURL mtrContentLauncherClusterLaunchURLParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchURLParams (mkSelector "setContentURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- displayString@
displayString :: IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams => mtrContentLauncherClusterLaunchURLParams -> IO (Id NSString)
displayString mtrContentLauncherClusterLaunchURLParams  =
    sendMsg mtrContentLauncherClusterLaunchURLParams (mkSelector "displayString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDisplayString:@
setDisplayString :: (IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams, IsNSString value) => mtrContentLauncherClusterLaunchURLParams -> value -> IO ()
setDisplayString mtrContentLauncherClusterLaunchURLParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchURLParams (mkSelector "setDisplayString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- brandingInformation@
brandingInformation :: IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams => mtrContentLauncherClusterLaunchURLParams -> IO (Id MTRContentLauncherClusterBrandingInformationStruct)
brandingInformation mtrContentLauncherClusterLaunchURLParams  =
    sendMsg mtrContentLauncherClusterLaunchURLParams (mkSelector "brandingInformation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBrandingInformation:@
setBrandingInformation :: (IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams, IsMTRContentLauncherClusterBrandingInformationStruct value) => mtrContentLauncherClusterLaunchURLParams -> value -> IO ()
setBrandingInformation mtrContentLauncherClusterLaunchURLParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchURLParams (mkSelector "setBrandingInformation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams => mtrContentLauncherClusterLaunchURLParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrContentLauncherClusterLaunchURLParams  =
    sendMsg mtrContentLauncherClusterLaunchURLParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams, IsNSNumber value) => mtrContentLauncherClusterLaunchURLParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrContentLauncherClusterLaunchURLParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchURLParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams => mtrContentLauncherClusterLaunchURLParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrContentLauncherClusterLaunchURLParams  =
    sendMsg mtrContentLauncherClusterLaunchURLParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams, IsNSNumber value) => mtrContentLauncherClusterLaunchURLParams -> value -> IO ()
setServerSideProcessingTimeout mtrContentLauncherClusterLaunchURLParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchURLParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentURL@
contentURLSelector :: Selector
contentURLSelector = mkSelector "contentURL"

-- | @Selector@ for @setContentURL:@
setContentURLSelector :: Selector
setContentURLSelector = mkSelector "setContentURL:"

-- | @Selector@ for @displayString@
displayStringSelector :: Selector
displayStringSelector = mkSelector "displayString"

-- | @Selector@ for @setDisplayString:@
setDisplayStringSelector :: Selector
setDisplayStringSelector = mkSelector "setDisplayString:"

-- | @Selector@ for @brandingInformation@
brandingInformationSelector :: Selector
brandingInformationSelector = mkSelector "brandingInformation"

-- | @Selector@ for @setBrandingInformation:@
setBrandingInformationSelector :: Selector
setBrandingInformationSelector = mkSelector "setBrandingInformation:"

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

