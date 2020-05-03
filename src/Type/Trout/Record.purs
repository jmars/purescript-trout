module Type.Trout.Record (get, set, insert, delete) where

import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Prim.Row (class Cons)
import Record.Unsafe (unsafeDelete, unsafeGet, unsafeSet)

get
  :: forall r r' l a
   . IsSymbol l
  => Cons l a r' r
  => SProxy l
  -> Record r
  -> a
get l = unsafeGet (reflectSymbol l)

set
  :: forall r1 r2 r l a b
   . IsSymbol l
  => Cons l a r r1
  => Cons l b r r2
  => SProxy l
  -> b
  -> Record r1
  -> Record r2
set l = unsafeSet (reflectSymbol l)

insert
  :: forall r1 r2 l a
   . IsSymbol l
  => Cons l a r1 r2
  => SProxy l
  -> a
  -> Record r1
  -> Record r2
insert l = unsafeSet (reflectSymbol l)

delete
  :: forall r1 r2 l a
   . IsSymbol l
  => Cons l a r2 r1
  => SProxy l
  -> Record r1
  -> Record r2
delete l = unsafeDelete (reflectSymbol l)
