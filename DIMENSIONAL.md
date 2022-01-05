# The `dimensional` package

- We use `coulomb` for unit analysis.
- `coulomb` uses static types to represent units and this information doesn't make it to runtime.
  - Eg:
- Sometimes we do need units to be represented at runtime. For example, we may want want to store what units a user chose.
  - This package attempts to reuse `coulomb` unit definitions to provide runtime units representation.

## Overview and relationship to `coulomb`

In `coulomb`, a `Quantity[N, U]` is a wrapper over a `value: N` and the units (`U`) information is lost at runtime:

```scala
scala> val velocity = 10.withUnit[Meter %/ Second]
val velocity: coulomb.Quantity[Int,coulomb.si.Meter %/ coulomb.si.Second] = Quantity(10)
```

In `dimensional`, a `Measure[N]` is similar but with a units represented in a value rather than in a type:

```scala
scala> val velocityM = velocity.toMeasure
val velocityM: lucuma.core.math.dimensional.Measure[Int] = Measure(10,m/s)
```

The runtime representation of units of measure is the class `Units`. Instances of `Units` are automatically derived for types `U` with instances of `coulomb`'s `BaseUnit[U]`, `DerivedUnit[U, D]`, or compound combinations of these by using the `%*`, `%/` and `%^` combinators.

## Tagged units

Sometimes we want to group units together. We can do this by assigning them a type tag via implicit `TaggedUnit[U, T]` instances.

```scala
scala> trait Velocity

scala> implicit val MPerS_Velocity = new TaggedUnit[Meter %/ Second, Velocity]

scala> val velocityMTagged: Measure[Int] Of Velocity = velocity.toMeasureTagged
val velocityMTagged: lucuma.core.math.dimensional.Of[lucuma.core.math.dimensional.Measure[Int],Velocity] = Measure(10,m/s)
```

This is usually not necessary. `coulomb` allows us to define conversions between units, and we don't need to explicitly define abstract quantities (like velocity). However, in some cases like brightness, we may want to group together units that are not readily convertible among them without further information.

`Units` can be tagged too. We can access the tagged `Units` of a tagged `Measure` by using the `unitsTagged` lens:

```scala
scala> val unitsTagged: Units Of Velocity = Measure.unitsTagged.get(velocityMTagged)
val unitsTagged: lucuma.core.math.dimensional.Of[lucuma.core.math.dimensional.Units,Velocity] = m/s
```

### Final note

- No numeric operations are supported on `Measure`, but they could be implemented. (A `Measure` can be converted to a `Quantity` and back).
