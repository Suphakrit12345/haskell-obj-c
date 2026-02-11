{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAttributeReport@.
module ObjC.Matter.MTRAttributeReport
  ( MTRAttributeReport
  , IsMTRAttributeReport(..)
  , initWithResponseValue_error
  , path
  , value
  , error_
  , initWithResponseValue_errorSelector
  , pathSelector
  , valueSelector
  , errorSelector


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

-- | Initialize an MTRAttributeReport with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not an attribute response.
--
-- Will set the value property to nil and the error property to non-nil, even if the schema for the value is not known, if the response-value is an error, not data.
--
-- Will return nil and hand out an error if the response-value is data in the following cases:
--
-- * The response is for a cluster/attribute combination for which the schema is   unknown and hence the type of the data is not known. * The data does not match the known schema.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRAttributeReport mtrAttributeReport, IsNSDictionary responseValue, IsNSError error_) => mtrAttributeReport -> responseValue -> error_ -> IO (Id MTRAttributeReport)
initWithResponseValue_error mtrAttributeReport  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrAttributeReport (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- path@
path :: IsMTRAttributeReport mtrAttributeReport => mtrAttributeReport -> IO (Id MTRAttributePath)
path mtrAttributeReport  =
    sendMsg mtrAttributeReport (mkSelector "path") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | value will be nil in the following cases:
--
-- * There was an error.  In this case, "error" will not be nil. * The attribute is nullable and the value of the attribute is null.
--
-- If value is not nil, the actual type of value will depend on the schema-defined (typically defined in the Matter specification) type of the attribute as follows:
--
-- * list: NSArray of whatever type the list entries are. * struct: The corresponding structure interface defined by Matter.framework * octet string: NSData * string: NSString * discrete/analog types: NSNumber
--
-- Derived types (in the Matter specification sense) are represented the same as the base type, except for "string" (which is a derived type of "octet string" in the specification).
--
-- ObjC selector: @- value@
value :: IsMTRAttributeReport mtrAttributeReport => mtrAttributeReport -> IO RawId
value mtrAttributeReport  =
    fmap (RawId . castPtr) $ sendMsg mtrAttributeReport (mkSelector "value") (retPtr retVoid) []

-- | If this specific path resulted in an error, the error (in the MTRInteractionErrorDomain or MTRErrorDomain) that corresponds to this path.
--
-- ObjC selector: @- error@
error_ :: IsMTRAttributeReport mtrAttributeReport => mtrAttributeReport -> IO (Id NSError)
error_ mtrAttributeReport  =
    sendMsg mtrAttributeReport (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

