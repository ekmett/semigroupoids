semigroupoids
=============

[![Hackage](https://img.shields.io/hackage/v/semigroupoids.svg)](https://hackage.haskell.org/package/semigroupoids) [![Build Status](https://github.com/ekmett/semigroupoids/workflows/Haskell-CI/badge.svg)](https://github.com/ekmett/semigroupoids/actions?query=workflow%3AHaskell-CI)

A semigroupoid is a `Category` without `id`. This package provides a range of 
`id`-free versions of type classes, as well as some supporting functions and
data types.

Field Guide
-----------

The diagram below describes the relationships between the type classes defined
in this package, and those from `base` (with some from `contravariant` as well). Thick-bordered
nodes correspond to type classes defined in this package; thin-bordered ones are
from elsewhere. Solid edges represent subclass relationships that actually
exist; dashed edges are those which _should_ exist in theory.

![A diagram of the relationships between type classes defined in this package and elsewhere.](https://raw.github.com/ekmett/semigroupoids/master/img/classes.svg)

We also provide the following table. This is structured in superclass order -
thus, for any type class `T`, all superclasses of `T` will be listed before `T`
in the table.

|**Name**|**Location**|**Superclass of**|**Ideally superclass of**|
|--------|------------|-----------------|-------------------------|
|`Functor`|`base`|`Alt`, `Apply`, `Traversable`||
|`Foldable`|`base`|`Traversable`, `Foldable1`||
|`Bifunctor`|`base`|`Biapply`||
|`Contravariant`|`base`|`Divise`, `Decide`||
|`Semigroupoid`|`semigroupoids`||`Category`|
|`Alt`|`semigroupoids`|`Plus`||
|`Apply`|`semigroupoids`|`Bind`|`Applicative`|
|`Traversable`|`base`|`Traversable1`||
|`Foldable1`|`semigroupoids`|`Traversable1`||
|`Biapply`|`semigroupoids`|||
|`Divise`|`semigroupoids`||`Divisible`|
|`Decide`|`semigroupoids`|`Conclude`|`Decidable`|
|`Category`|`base`|`Arrow`||
|`Plus`|`semigroupoids`||`Alternative`|
|`Applicative`|`base`|`Alternative`, `Monad`||
|`Bind`|`semigroupoids`||`Monad`|
|`Traversable1`|`semigroupoids`|||
|`Divisible`|`contravariant`|||
|`Conclude`|`semigroupoids`||`Decidable`|
|`Arrow`|`base`|||
|`Alternative`|`base`|`MonadPlus`||
|`Monad`|`base`|`MonadPlus`||
|`Decidable`|`contravariant`|||
|`MonadPlus`|`base`|||

We omit some type class relationships from this diagram, as they are not
relevant for the purposes of this package.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through Github or on the #haskell IRC channel on
LiberaChat.

-Edward Kmett
