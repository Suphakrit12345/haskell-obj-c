{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Meta-data about an endpoint of a Matter node.
--
-- Generated bindings for @MTREndpointInfo@.
module ObjC.Matter.MTREndpointInfo
  ( MTREndpointInfo
  , IsMTREndpointInfo(..)
  , init_
  , new
  , endpointID
  , deviceTypes
  , partsList
  , children
  , initSelector
  , newSelector
  , endpointIDSelector
  , deviceTypesSelector
  , partsListSelector
  , childrenSelector


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

-- | @- init@
init_ :: IsMTREndpointInfo mtrEndpointInfo => mtrEndpointInfo -> IO (Id MTREndpointInfo)
init_ mtrEndpointInfo  =
    sendMsg mtrEndpointInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTREndpointInfo)
new  =
  do
    cls' <- getRequiredClass "MTREndpointInfo"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- endpointID@
endpointID :: IsMTREndpointInfo mtrEndpointInfo => mtrEndpointInfo -> IO (Id NSNumber)
endpointID mtrEndpointInfo  =
    sendMsg mtrEndpointInfo (mkSelector "endpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deviceTypes@
deviceTypes :: IsMTREndpointInfo mtrEndpointInfo => mtrEndpointInfo -> IO (Id NSArray)
deviceTypes mtrEndpointInfo  =
    sendMsg mtrEndpointInfo (mkSelector "deviceTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- partsList@
partsList :: IsMTREndpointInfo mtrEndpointInfo => mtrEndpointInfo -> IO (Id NSArray)
partsList mtrEndpointInfo  =
    sendMsg mtrEndpointInfo (mkSelector "partsList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The direct children of this endpoint. This excludes indirect descendants even if they are listed in the PartsList attribute of this endpoint due to the Full-Family Pattern being used. Refer to Endpoint Composition Patterns in the Matter specification for details.
--
-- ObjC selector: @- children@
children :: IsMTREndpointInfo mtrEndpointInfo => mtrEndpointInfo -> IO (Id NSArray)
children mtrEndpointInfo  =
    sendMsg mtrEndpointInfo (mkSelector "children") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @deviceTypes@
deviceTypesSelector :: Selector
deviceTypesSelector = mkSelector "deviceTypes"

-- | @Selector@ for @partsList@
partsListSelector :: Selector
partsListSelector = mkSelector "partsList"

-- | @Selector@ for @children@
childrenSelector :: Selector
childrenSelector = mkSelector "children"

