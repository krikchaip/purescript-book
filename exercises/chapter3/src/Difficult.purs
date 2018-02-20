module Difficult where

import Data.AddressBook (AddressBook, insertEntry)
import Prelude

import Control.Plus (empty)
import Data.List (nubBy)

address = { street: "123 Fake St.", city: "Faketown", state: "CA" }
entry = { firstName: "John", lastName: "Smith", address: address }
entry2 = { firstName: "Winner", lastName: "PNG", address: address }
book1 = insertEntry entry $ insertEntry entry $ insertEntry entry2 empty

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates =
  nubBy (\entryA entryB ->
    entryA.firstName == entryB.firstName &&
    entryA.lastName == entryB.lastName)