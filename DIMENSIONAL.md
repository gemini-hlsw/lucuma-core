# The `dimensional` package

- We use `coulomb` for unit analysis.
- `coulomb` uses static types to represent units and this information doesn't make it to runtime.
- Sometimes we do need units to be represented at runtime. For example, we may want want to store what units a user chose.
  - This package attempts to reuse `coulomb` unit definitions to provide runtime units representation.

## Overview and relationship to `coulomb`

### `Measure[N]`

- In `coulomb`, a `Quantity[N, U]` is a wrapper over a `value: N` and the units (`U`) information is lost at runtime:

```scala
scala> val velocity = 10.withUnit[Meter %/ Second]
val velocity: coulomb.Quantity[Int,coulomb.si.Meter %/ coulomb.si.Second] = Quantity(10)
```

- In `dimensional`, a `Measure[N]` is similar but with a units represented in a value rather than in a type:

```scala
scala> val velocityM = velocity.toMeasure
val velocityM: lucuma.core.math.dimensional.Measure[Int] = Measure(10,m/s)
```

### `Units` and `UnitOfMeasure[U]`

- The runtime representation of units of measure is the class `Units`. `Units` is _not_ type-parametrized.

- `UnitOfMeasure[U]` is a subtype of `Units` which is type-parametrized on the unit type `U`.
  - It can be used to automatically derive a `Units` instance for a type `U` which has an instance of `coulomb`'s `BaseUnit[U]`, `DerivedUnit[U, D]`, or a compound combination of these by using the `%*`, `%/` and `%^` combinators:

```scala
scala> val units: Units = UnitOfMeasure[Meter %/ Second]
val units: lucuma.core.math.dimensional.Units = m/s
```

- If we have a `Units`, we can create a `Measure` by using `withValue`:

```scala
scala> val measure = units.withValue(10)
val measure: lucuma.core.math.dimensional.Measure[Int] = Measure(10,m/s)
```

## Tagged measures and units

- Sometimes we want to group units together. We can do this by assigning them a type tag via implicit `TaggedUnit[U, T]` instances:

```scala
scala> trait Velocity

scala> implicit val MPerS_Velocity = new TaggedUnit[Meter %/ Second, Velocity]

scala> val velocityMTagged: Measure[Int] Of Velocity = velocity.toMeasureTagged
val velocityMTagged: lucuma.core.math.dimensional.Of[lucuma.core.math.dimensional.Measure[Int],Velocity] = Measure(10,m/s)
```

This is usually not necessary. `coulomb` allows us to define conversions between units, and we don't need to explicitly define abstract quantities (like velocity). However, in some cases like brightness, we may want to group together units that are not readily convertible among them without further information.

- `Units` can be tagged too. We can access the tagged `Units` of a tagged `Measure` by using the `unitsTagged` lens:

```scala
scala> val unitsTagged: Units Of Velocity = Measure.unitsTagged.get(velocityMTagged)
val unitsTagged: lucuma.core.math.dimensional.Of[lucuma.core.math.dimensional.Units,Velocity] = m/s
```

- If we have a `Units Of Tag`, we can create a `Measure Of Tag` by using `withValueTagged`:

```scala
scala> val velocityMTagged2: Measure[Int] Of Velocity = unitsTagged.withValueTagged(20)
val velocityMTagged2: lucuma.core.math.dimensional.Of[lucuma.core.math.dimensional.Measure[Int],Velocity] = Measure(20,m/s)
```

### Final note

- `coulomb` supports numeric operations between `Quantity` instances (eg: order, addition, negation, etc.) by relying on a set of typeclasses (`UnitOrd`, `UnitAdd`, etc.). `Measure` does not support this for the moment, but it could probably be implemented if needed, reusing `coulomb`'s typeclasses.
