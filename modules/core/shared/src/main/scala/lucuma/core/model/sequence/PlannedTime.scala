// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.Monoid
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.semigroup.*
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ObserveClass
import lucuma.core.util.TimeSpan

import scala.annotation.targetName
import scala.collection.immutable.SortedMap

opaque type PlannedTime = SortedMap[ChargeClass, TimeSpan]

/**
 * Planned time broken down by charge class.
 */
object PlannedTime {

  val Zero: PlannedTime =
    SortedMap.empty

  def apply(charges: (ChargeClass, TimeSpan)*): PlannedTime =
    SortedMap(charges*)

  def from(it: IterableOnce[(ChargeClass, TimeSpan)]): PlannedTime =
    SortedMap.from(it)

  /**
   * Creates a `PlannedTime` instance where the entirety of the step estimate
   * is associated with the `ObserveClass`'s `ChargeClass`.
   */
  def fromStep(observeClass: ObserveClass, stepEstimate: StepEstimate): PlannedTime =
    apply(observeClass.chargeClass -> stepEstimate.total)

  extension (pt: PlannedTime) {

    /**
     * Gets the time charged for the given charge class (or TimeSpan.Zero if no
     * charge is recorded).  Alias for `getOrZero`.
     */
    def apply(chargeClass: ChargeClass): TimeSpan =
      getOrZero(chargeClass)

    // N.B., if you refer to `apply` within this extensions object, it will
    // find the Map apply :-/ For that reason I added `getOrZero`.

    /**
     * Gets the time charged for the given charge class (or TimeSpan.Zero if no
     * charge is recorded).
     */
    def getOrZero(chargeClass: ChargeClass): TimeSpan =
      pt.getOrElse(chargeClass, TimeSpan.Zero)

    /**
     * Returns `true` if there are no charges for any charge class.
     */
    def isZero: Boolean =
      pt.forall(_._2.toMicroseconds === 0)

    /**
     * Returns `true` if there are is a charge for at least one charge class.
     */
    def nonZero: Boolean =
      !isZero

    /**
     * Returns an updated PlannedTime value, changing the charge associated
     * with `chargeClass` to `time`.
     */
    def updated(chargeClass: ChargeClass, time: TimeSpan): PlannedTime =
      pt.updated(chargeClass, time)

    /**
     * Sums the current charge associated with `chargeClass` with the given
     * `time`, returning a new PlannedTime value.
     */
    def sumCharge(chargeClass: ChargeClass, time: TimeSpan): PlannedTime =
      pt.updated(chargeClass, getOrZero(chargeClass) +| time)

    /**
     * Sums all the charges regardless of charge class.
     */
    def sum: TimeSpan =
      ChargeClass.values.toList.foldMap(getOrZero)

    /**
     * Lists the charge classes and their time value.
     */
    def charges: List[(ChargeClass, TimeSpan)] =
      ChargeClass.values.toList.fproduct(getOrZero)

    /**
     * Adds the corresponding charges for two PlannedTime values.
     */
    @targetName("boundedAdd")
    def +|(other: PlannedTime): PlannedTime =
      other.charges.foldLeft(pt) { case (res, (chargeClass, time)) =>
        res.sumCharge(chargeClass, time)
      }
  }

  given Monoid[PlannedTime] =
    Monoid.instance(Zero, _ +| _)

  given Eq[PlannedTime] =
    Eq.by(_.charges)

}