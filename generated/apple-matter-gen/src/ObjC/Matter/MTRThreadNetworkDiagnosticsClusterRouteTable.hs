{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterRouteTable@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterRouteTable
  ( MTRThreadNetworkDiagnosticsClusterRouteTable
  , IsMTRThreadNetworkDiagnosticsClusterRouteTable(..)
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
extAddress :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
extAddress mtrThreadNetworkDiagnosticsClusterRouteTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "extAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtAddress:@
setExtAddress :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setExtAddress mtrThreadNetworkDiagnosticsClusterRouteTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "setExtAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rloc16@
rloc16 :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
rloc16 mtrThreadNetworkDiagnosticsClusterRouteTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "rloc16") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRloc16:@
setRloc16 :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setRloc16 mtrThreadNetworkDiagnosticsClusterRouteTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "setRloc16:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- routerId@
routerId :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
routerId mtrThreadNetworkDiagnosticsClusterRouteTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "routerId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRouterId:@
setRouterId :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setRouterId mtrThreadNetworkDiagnosticsClusterRouteTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "setRouterId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nextHop@
nextHop :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
nextHop mtrThreadNetworkDiagnosticsClusterRouteTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "nextHop") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNextHop:@
setNextHop :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setNextHop mtrThreadNetworkDiagnosticsClusterRouteTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "setNextHop:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pathCost@
pathCost :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
pathCost mtrThreadNetworkDiagnosticsClusterRouteTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "pathCost") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPathCost:@
setPathCost :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setPathCost mtrThreadNetworkDiagnosticsClusterRouteTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "setPathCost:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lqiIn@
lqiIn :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
lqiIn mtrThreadNetworkDiagnosticsClusterRouteTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "lqiIn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLqiIn:@
setLqiIn :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setLqiIn mtrThreadNetworkDiagnosticsClusterRouteTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "setLqiIn:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lqiOut@
lqiOut :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
lqiOut mtrThreadNetworkDiagnosticsClusterRouteTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "lqiOut") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLqiOut:@
setLqiOut :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setLqiOut mtrThreadNetworkDiagnosticsClusterRouteTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "setLqiOut:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- age@
age :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
age mtrThreadNetworkDiagnosticsClusterRouteTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "age") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAge:@
setAge :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setAge mtrThreadNetworkDiagnosticsClusterRouteTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "setAge:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allocated@
allocated :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
allocated mtrThreadNetworkDiagnosticsClusterRouteTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "allocated") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setAllocated:@
setAllocated :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setAllocated mtrThreadNetworkDiagnosticsClusterRouteTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "setAllocated:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- linkEstablished@
linkEstablished :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
linkEstablished mtrThreadNetworkDiagnosticsClusterRouteTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "linkEstablished") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLinkEstablished:@
setLinkEstablished :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setLinkEstablished mtrThreadNetworkDiagnosticsClusterRouteTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterRouteTable (mkSelector "setLinkEstablished:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

