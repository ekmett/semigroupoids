5.3.4 [2019.11.26]
------------------
* Achieve forward compatibility with
  [GHC proposal 229](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst).

5.3.3 [2019.08.27]
------------------
* Add `Alt` and `Plus` instances for `HashMap` from the `unordered-containers`
  package.

5.3.2 [2019.01.04]
------------------
* Bump the lower bound on `semigroups` to 0.16.2, and avoid incurring
  the dependency entirely on recent GHCs.
* Fix the build on GHC 7.0 and 7.2.

5.3.1 [2018.07.02]
------------------
* Fix a regression introduced in `semigroupoids-5.3` in which some modules
  regressed from `Trustworthy` to `Unsafe`.

5.3 [2018.07.02]
----------------
* Allow building with `containers-0.6`.
* Add `Alt` instances for `First` and `Last` from `Data.Semigroup`, and
  `Alt` and `Plus` instances for `First` and `Last` from `Data.Monoid`.
* Add missing `Apply`, `Bind`, `Extend`, `Foldable1` and `Traversable1`
  instances for `Data.Semigroups`, `Data.Monoid` and `GHC.Generics`.

5.2.2 [2018.01.18]
------------------
* Add `optional` to `Data.Functor.Alt` (analogous to the `optional` function
  in `Control.Applicative`)
* `liftF2` is now a class method of `Apply` (mirroring the fact that `liftA2`
  is now a class method of `Applicative`). `liftF2` and `(<.>)` have default
  definitions in terms of the other.
* Allow building with GHC 8.4
* `Apply` and `Bind` instances for `Q`, from the `template-haskell` package.
  (As a consequence, `Data.Semigroup.Foldable` is no longer a `Trustworthy`
  module.)
* Add instances for `(:~:)` and `(:~~:)` from `Data.Type.Equality`, and
  `Coercion` from `Data.Type.Coercion`

5.2.1
-----
* Add the `toNonEmpty` method to `Foldable1`. Add `foldrM1` and `foldlM1`
  functions to `Data.Semigroup.Foldable` that are defined in terms of `toNonEmpty`.
* Add `Apply`, `Bind`, `Foldable1`, and `Traversable1` instances for `Complex`
* Add `Apply` and `Bind` instances for `HashMap` from the `unordered-containers` package
  (on which `semigroupoids` now depends)
* Add `Semigroupoid` instances for `Tagged` and `Const`

5.2
---
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-1.25`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.
* Added instances to `Alt`, `Plus`, `Apply`, `Bind` and `Extend` for `GHC.Generics`, `Tagged` and `Proxy` where appropriate.

5.1
---
* The remaining orphan instances in `Data.Traversable.Instances` have been replaced in favor of the orphan instances from `transformers-compat-0.5`.
* The documentation now states laws that instances of `Apply` are expected to uphold.
* `doctest-0.11` support
* Fixed compilation of tests with `stack`

5.0.1
-------
* `transformers-compat` 0.5 support
* Removed some redundant constraints.
* GHC 8 support

5.0.0.4
-------
* `doctest` 0.10 support

5.0.0.2
-------
* Bugfix for GHC 7.4. PolyKinds on 7.4 cause all sorts of haskell interface file errors. One of the #if guards that turned it off on 7.4 was missing and has been fixed.

5.0.0.1
-------
* Added the CHANGELOG to the distribution so that `hackage` can link to it in the haddocks.

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
