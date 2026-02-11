{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterRouteTableStruct@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterRouteTableStruct
  ( MTRThreadNetworkDiagnosticsClusterRouteTableStruct
  , IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct(..)
  , extAddress
  , setExtAddress
  , rloc16
  , setRloc16
  , routerId
  , setRouterId
  , nextHop
  , setNextHop
  , pathCost
  , setPathCost
  , lqiIn
  , setLqiIn
  , lqiOut
  , setLqiOut
  , age
  , setAge
  , allocated
  , setAllocated
  , linkEstablished
  , setLinkEstablished
  , extAddressSelector
  , setExtAddressSelector
  , rloc16Selector
  , setRloc16Selector
  , routerIdSelector
  , setRouterIdSelector
  , nextHopSelector
  , setNextHopSelector
  , pathCostSelector
  , setPathCostSelector
  , lqiInSelector
  , setLqiInSelector
  , lqiOutSelector
  , setLqiOutSelector
  , ageSelector
  , setAgeSelector
  , allocatedSelector
  , setAllocatedSelector
  , linkEstablishedSelector
  , setLinkEstablishedSelector


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

-- | @- extAddress@
extAddress :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
extAddress mtrThreadNetworkDiagnosticsClusterRouteTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "extAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtAddress:@
setExtAddress :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setExtAddress mtrThreadNetworkDiagnosticsClusterRouteTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "setExtAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rloc16@
rloc16 :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
rloc16 mtrThreadNetworkDiagnosticsClusterRouteTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "rloc16") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRloc16:@
setRloc16 :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setRloc16 mtrThreadNetworkDiagnosticsClusterRouteTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "setRloc16:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- routerId@
routerId :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
routerId mtrThreadNetworkDiagnosticsClusterRouteTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "routerId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRouterId:@
setRouterId :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setRouterId mtrThreadNetworkDiagnosticsClusterRouteTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "setRouterId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nextHop@
nextHop :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
nextHop mtrThreadNetworkDiagnosticsClusterRouteTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "nextHop") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNextHop:@
setNextHop :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setNextHop mtrThreadNetworkDiagnosticsClusterRouteTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "setNextHop:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pathCost@
pathCost :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
pathCost mtrThreadNetworkDiagnosticsClusterRouteTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "pathCost") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPathCost:@
setPathCost :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setPathCost mtrThreadNetworkDiagnosticsClusterRouteTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "setPathCost:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lqiIn@
lqiIn :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
lqiIn mtrThreadNetworkDiagnosticsClusterRouteTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "lqiIn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLqiIn:@
setLqiIn :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setLqiIn mtrThreadNetworkDiagnosticsClusterRouteTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "setLqiIn:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lqiOut@
lqiOut :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
lqiOut mtrThreadNetworkDiagnosticsClusterRouteTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "lqiOut") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLqiOut:@
setLqiOut :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setLqiOut mtrThreadNetworkDiagnosticsClusterRouteTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "setLqiOut:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- age@
age :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
age mtrThreadNetworkDiagnosticsClusterRouteTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "age") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAge:@
setAge :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setAge mtrThreadNetworkDiagnosticsClusterRouteTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "setAge:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allocated@
allocated :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
allocated mtrThreadNetworkDiagnosticsClusterRouteTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "allocated") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setAllocated:@
setAllocated :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setAllocated mtrThreadNetworkDiagnosticsClusterRouteTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "setAllocated:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- linkEstablished@
linkEstablished :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
linkEstablished mtrThreadNetworkDiagnosticsClusterRouteTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "linkEstablished") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLinkEstablished:@
setLinkEstablished :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setLinkEstablished mtrThreadNetworkDiagnosticsClusterRouteTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTableStruct (mkSelector "setLinkEstablished:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @extAddress@
extAddressSelector :: Selector
extAddressSelector = mkSelector "extAddress"

-- | @Selector@ for @setExtAddress:@
setExtAddressSelector :: Selector
setExtAddressSelector = mkSelector "setExtAddress:"

-- | @Selector@ for @rloc16@
rloc16Selector :: Selector
rloc16Selector = mkSelector "rloc16"

-- | @Selector@ for @setRloc16:@
setRloc16Selector :: Selector
setRloc16Selector = mkSelector "setRloc16:"

-- | @Selector@ for @routerId@
routerIdSelector :: Selector
routerIdSelector = mkSelector "routerId"

-- | @Selector@ for @setRouterId:@
setRouterIdSelector :: Selector
setRouterIdSelector = mkSelector "setRouterId:"

-- | @Selector@ for @nextHop@
nextHopSelector :: Selector
nextHopSelector = mkSelector "nextHop"

-- | @Selector@ for @setNextHop:@
setNextHopSelector :: Selector
setNextHopSelector = mkSelector "setNextHop:"

-- | @Selector@ for @pathCost@
pathCostSelector :: Selector
pathCostSelector = mkSelector "pathCost"

-- | @Selector@ for @setPathCost:@
setPathCostSelector :: Selector
setPathCostSelector = mkSelector "setPathCost:"

-- | @Selector@ for @lqiIn@
lqiInSelector :: Selector
lqiInSelector = mkSelector "lqiIn"

-- | @Selector@ for @setLqiIn:@
setLqiInSelector :: Selector
setLqiInSelector = mkSelector "setLqiIn:"

-- | @Selector@ for @lqiOut@
lqiOutSelector :: Selector
lqiOutSelector = mkSelector "lqiOut"

-- | @Selector@ for @setLqiOut:@
setLqiOutSelector :: Selector
setLqiOutSelector = mkSelector "setLqiOut:"

-- | @Selector@ for @age@
ageSelector :: Selector
ageSelector = mkSelector "age"

-- | @Selector@ for @setAge:@
setAgeSelector :: Selector
setAgeSelector = mkSelector "setAge:"

-- | @Selector@ for @allocated@
allocatedSelector :: Selector
allocatedSelector = mkSelector "allocated"

-- | @Selector@ for @setAllocated:@
setAllocatedSelector :: Selector
setAllocatedSelector = mkSelector "setAllocated:"

-- | @Selector@ for @linkEstablished@
linkEstablishedSelector :: Selector
linkEstablishedSelector = mkSelector "linkEstablished"

-- | @Selector@ for @setLinkEstablished:@
setLinkEstablishedSelector :: Selector
setLinkEstablishedSelector = mkSelector "setLinkEstablished:"

