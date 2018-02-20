module Part2 where

import Prelude

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

james :: Person
james = { name: "James", address: { street: "101 Rd.", city: "LA" } }

winner :: Person
winner = { name: "Winner", address: { street: "102 Rd.", city: "Lc" } }

-- easy
sameCity :: Person -> Person -> Boolean
sameCity p1 p2 = p1.address.city == p2.address.city

-- medium
-- sameCity :: forall t3 t5 t6 t8 t9. Eq t3 =>
--   { address :: { city :: t3 | t5 } | t6 } ->
--   { address :: { city :: t3 | t8 } | t9 } -> Boolean
-- livesInLA :: forall t0 t1.
--   { address :: { city :: String | t1 } | t0 } -> Boolean

-- medium
fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton default _ = default