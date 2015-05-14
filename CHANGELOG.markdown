5
-
* Absorbed `Data.Bifunctor.Apply`, `Data.Semigroup.Bifoldable` and `Data.Semigroup.Traversable` from `bifunctors`.
* This caused us to pick up a dependency on `tagged`.
* Exiled `Data.Semifunctor.*`, `Data.Semigroupoid.Product` and `Data.Semigroupoid.Coproduct` to `semigroupoid-extras`.
* This let us open up to older versions of GHC again.
* Set an explicit fixity for `-<-` and `->-`.

4.5
---
* Major changes to the API to support PolyKinds and DataKinds. This necessarily shuts off GHC <= 7.4.
* Orphan instances have moved upstream into a common `base-orphans` package.

4.3.1
-----
* Added `asum1` to `Data.Semigroup.Foldable`.

4.3.0.1
-------
* Support for 'ConstrainedClassMethods' is currently required for GHC HEAD.

4.3
-----
* Added missing instances for `ExceptT`. Obtain it via `transformers-compat` if need be for old `transformers` versions.
* Several `Bind` and `Apply` instances now require somewhat more minimal contexts.

4.2
---
* Backported `Foldable`/`Traversable` instances from `lens`

4.1
---
* `Foldable1`/`Traversable1` for tuples

4.0.4
-----
* `contravariant` 1.0 support.

4.0.3
---
* Added flags to provide unsupported cabal sandbox build modes.

4.0.1
-----
* Fixed bitrot in the `Data.Functor.Extend` documentation.
* Fixed warnings on GHC 7.8.1rc2 caused by importing `Control.Monad.Instances`.

4.0
---
* Merged in the contents of the `groupoids` and `semigroupoid-extras` packages.

3.1
---
* Added the [rectangular band](http://en.wikipedia.org/wiki/Band_(mathematics)#Rectangular_bands) `Semigroupoid` for `(,)`. Would that make it a Bandoid?

3.0.3
-----
* Claim to be `Trustworthy` where necessary

3.0.2
-----
* Tightened the upper bounds slightly to enable PVP compliance while retaining a flexible development cycle.
* Raised the upper bound on `contravariant`.

3.0.1
-----
* Removed upper bounds relative to my other packages
* Refactored directory layout
