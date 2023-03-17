6.0.0.1 [2023.03.16]
--------------------
* When building with GHC 9.6, require `transformers >= 0.6.1` and
  `containers >= 0.6.7`. This ensures that `semigroupoids` always provides
  `Traversable1` instances for data types from `transformers` and `containers`
  unconditionally.

6 [2023.03.12]
--------------
* Drop support for GHC 7.10 and earlier.
* The `Foldable1` and `Bifoldable1` classes have been migrated:
  * When building with `base-4.18` or later, `semigroupoids` re-exports
    `Foldable1` and `Bifoldable1` from `base`. (These classes were added to
    `base-4.18` as a result of
    [this Core Libraries proposal](haskell/core-libraries-committee#9).)
  * When building with older versions of `base`, `semigroupoids` re-exports
    `Foldable1` and `Bifoldable1` from the
    [`foldable1-classes-compat`](https://github.com/haskell-compat/foldable1-classes-compat)
    compatibility package.

  Note that the version of `Foldable1` that `semigroupoids` defined in previous
  releases only had three class methods: `fold1`, `foldMap1`, and `toNonEmpty`.
  Moreover, `foldMap1` had a default implementation in terms of a `Foldable`
  constraint. `base`'s version of `Foldable1`, however, has some notable
  differences:

  1. It has many more methods than the three listed above, such as the
     `foldrMap1` method.
  2. `foldMap1` now has a default implementation in terms of `foldrMap1` instead
     of in terms of a `Foldable` constraint.

  To avoid (1) causing issues when upgrading to `semigroupoids-6`,
  `Data.Semigroup.Foldable` only re-exports the `fold1`, `foldMap1`, and
  `toNonEmpty` methods, which reflects the API in previous `semigroupoids`
  releases. If you want to use the other, new class methods of `Foldable1`,
  consider importing it from `Data.Foldable1` (its home in `base`) instead.

  Difference (2) is trickier, because it is possible that existing code that
  defines valid `Foldable1` instances will need to be migrated. If you have an
  instance like this:

  ```hs
  import Data.Semigroup.Foldable

  data T a = MkT a

  instance Foldable T where
    foldMap f (MkT x) = f x

  instance Foldable1 T -- Relying on Foldable-based defaults
  ```

  Then calling `foldMap1` on `T` will throw an error with `semigroupoids-6`, as
  `foldMap1`'s default implementation no longer uses `Foldable`. To migrate this
  code, change the instance to explicitly define `foldMap1`:

  ```hs
  instance Foldable1 T where
    foldMap1 f (MkT x) = f x
  ```

  This approach should be backwards-compatible with previous `semigroupoids`
  releases.

  Some other side effects of this migration include:

  * The `Data.Semigroup.Foldable.Class` module has been deprecated. It no
    longer serves a useful role, as it simply re-exports a limited subset of
    the `Data.Foldable1` and `Data.Bifoldable1` API.
  * All of the `Foldable1` and `Bifoldable1` instances that were previously
    defined in `semigroupoids` have now been migrated to downstream libraries
    (`base`, `bifunctors`, `containers`, `tagged`, and `transformers`), so it
    is no longer strictly necessary to depend on `semigroupoids` to make use of
    these instances.
* Add `Generic1`-based functions for many classes, useful for writing instances:
  - `Data.Functor.Alt.(<!>)` -> `Data.Functor.Alt.galt`
  - `Data.Functor.Apply.{liftF2,liftF3}` -> `Data.Functor.Apply.{gliftF2,gliftF3}`
  - `Data.Functor.Bind.(>>-)` -> `Data.Functor.Bind.gbind`
  - `Data.Functor.Contravariant.Conclude.{conclude,concluded}` -> `Data.Functor.Contravariant.Conclude.{gconclude,gconcluded}`
  - `Data.Functor.Contravariant.Decide.{decide,decided}` -> `Data.Functor.Contravariant.Decide.{gdecide,gdecided}`
  - `Data.Functor.Contravariant.Divise.{divise,divised}` -> `Data.Functor.Contravariant.Divise.{gdivise,gdivised}`
  - `Data.Functor.Extend.{duplicated,extended}` -> `Data.Functor.Extend.{gduplicated,gextended}`
  - `Data.Functor.Plus.zero` -> `Data.Functor.Plus.gzero`
  - `Data.Semigroup.Foldable.{fold1,foldMap1,toNonEmpty}` -> `Data.Semigroup.Foldable.{gfold1,gfoldMap1,gtoNonEmpty}`
  - `Data.Semigroup.Traversable.{traverse1,sequence1}` -> `Data.Semigroup.Traversable.{gtraverse1,gsequence1}`

5.3.7 [2022.01.09]
------------------
* Relax the `Bind` constraints in the following instances to `Functor`:

  ```diff
  -instance (Bind f,    Monad f) => Alt  (MaybeT f)
  -instance (Bind f,    Monad f) => Plus (MaybeT f)
  +instance (Functor f, Monad f) => Alt  (MaybeT f)
  +instance (Functor f, Monad f) => Plus (MaybeT f)

  -instance (Bind f,    Monad f, Semigroup e)           => Alt  (ExceptT e f)
  -instance (Bind f,    Monad f, Semigroup e, Monoid e) => Plus (ExceptT e f)
  +instance (Functor f, Monad f, Semigroup e)           => Alt  (ExceptT e f)
  +instance (Functor f, Monad f, Semigroup e, Monoid e) => Plus (ExceptT e f)

   -- If building with transformers-0.5.* or older
  -instance (Bind f,    Monad f)          => Alt  (ErrorT e f)
  -instance (Bind f,    Monad f, Error e) => Plus (ErrorT e f
  +instance (Functor f, Monad f)          => Alt  (ErrorT e f)
  +instance (Functor f, Monad f, Error e) => Plus (ErrorT e f)
  ```

5.3.6 [2021.10.07]
------------------
* Allow building with GHC 9.2.
* Allow building with `transformers-0.6.*`.
* Add `Alt` instance for `Identity`.
* Add `Conclude`, `Decide` and `Divise` type classes and instances.
* Add `(<.*>)`, `(<*.>)`, and `traverseMaybe` functions, which make it easier
  to defined `Traversable1` instances for data types that have fields with a
  combination of `Traversable` and `Traversable1` instances.
* Add `Semigroupoids.Do` module with overloads for use with `QualifiedDo`.
* Add `Apply`, `Alt`, `Plus`, `Bind` and `BindTrans` instances for the CPS
  versions of `WriterT` and `RWST`.
* Add `psum` function to `Data.Functor.Plus`.
* Add `Categorical` data type.

5.3.5 [2020.12.31]
------------------
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.
* Explicitly mark modules as `Safe`.

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
