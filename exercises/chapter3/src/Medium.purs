module Medium where

import Prelude

import Control.Plus (empty)
import Data.List (filter, head, null)
import Data.Maybe (Maybe)

import Data.AddressBook (Entry, AddressBook, showEntry, insertEntry)

address = { street: "123 Fake St.", city: "Faketown", state: "CA" }
entry = { firstName: "John", lastName: "Smith", address: address }
book1 = insertEntry entry empty

-- ex01
findStreet :: String -> AddressBook -> Maybe Entry
findStreet street =
  filter (\entry -> entry.address.street == street) >>> head

ans1_1 = map showEntry $ findStreet "123 Fake St." book1
ans1_2 = map showEntry $ findStreet "123" book1

-- ex02
nameExists :: String -> AddressBook -> Boolean
nameExists first =
  filter (\entry -> entry.firstName == first) >>> not null

ans2_1 = nameExists "John" book1
ans2_2 = nameExists "Winner" book1