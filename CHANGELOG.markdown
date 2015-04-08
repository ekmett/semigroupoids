4.4
---
* Added dependency on `base-compat`, which contains orphan `Foldable` and `Traversable` instances for `Either`, `Const`, and `(,)`.
* The `Data.Traversable.Instances` module now reexports the orphan instances from `base-compat`.

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
