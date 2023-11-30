// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import cats.implicits.catsKernelOrderingForOrder
import cats.kernel.CommutativeMonoid
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.order.*
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ObserveClass
import lucuma.core.util.TimeSpan

import scala.annotation.targetName
import scala.collection.immutable.SortedMap

/**
 * Time charge broken down by charge class.
 */
opaque type CategorizedTime = SortedMap[ChargeClass, TimeSpan]

object CategorizedTime {

  val Zero: CategorizedTime =
    SortedMap.empty

  def apply(charges: (ChargeClass, TimeSpan)*): CategorizedTime =
    SortedMap(charges*)

  def from(it: IterableOnce[(ChargeClass, TimeSpan)]): CategorizedTime =
    SortedMap.from(it)

  /**
   * Creates a `CategorizedTime` instance where the entirety of the step estimate
   * is associated with the `ObserveClass`'s `ChargeClass`.
   */
  def fromStep(observeClass: ObserveClass, stepEstimate: StepEstimate): CategorizedTime =
    apply(observeClass.chargeClass -> stepEstimate.total)

  extension (self: CategorizedTime) {

    /**
     * Gets the time charged for the given charge class (or TimeSpan.Zero if no
     * charge is recorded).  Alias for `getOrZero`.
     */
    def apply(chargeClass: ChargeClass): TimeSpan =
      getOrZero(chargeClass)

    // N.B., if you refer to `apply` within this extensions object, it will
    // find the Map apply :-/ For that reason I added `getOrZero`.

    /**
     * Adjusts this `CategorizedTime` instance according to the supplied
     * correction, adding or subtracting time associated its charge class but
     * bounded by the min and max `TimeSpan` values.
     */
    def correct(c: TimeChargeCorrection): CategorizedTime =
      c.op match {
        case TimeChargeCorrection.Op.Add      => modify(c.chargeClass, _ +| c.amount)
        case TimeChargeCorrection.Op.Subtract => modify(c.chargeClass, _ -| c.amount)
      }

    /**
     * Gets the time charged for the given charge class (or TimeSpan.Zero if no
     * charge is recorded).
     */
    def getOrZero(chargeClass: ChargeClass): TimeSpan =
      self.getOrElse(chargeClass, TimeSpan.Zero)

    /**
     * Returns `true` if there are no charges for any charge class.
     */
    def isZero: Boolean =
      self.forall(_._2.toMicroseconds === 0)

    /**
     * Returns `true` if there is a charge for at least one charge class.
     */
    def nonZero: Boolean =
      !isZero

    /**
     * Returns an updated CategorizedTime value, changing the charge associated
     * with `chargeClass` to `time`.
     */
    def updated(chargeClass: ChargeClass, time: TimeSpan): CategorizedTime =
      self.updated(chargeClass, time)

    /**
     * Modifies the amount associated with the given charge class using the
     * provided operation.
     */
    def modify(chargeClass: ChargeClass, op: TimeSpan => TimeSpan): CategorizedTime =
      self.updated(chargeClass, op(getOrZero(chargeClass)))

    /**
     * Sums the current charge associated with `chargeClass` with the given
     * `time`, returning a new CategorizedTime value.
     */
    def sumCharge(chargeClass: ChargeClass, time: TimeSpan): CategorizedTime =
      modify(chargeClass, _ +| time)

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
     * Adds the corresponding charges for two CategorizedTime values.
     */
    @targetName("boundedAdd")
    def +|(other: CategorizedTime): CategorizedTime =
      other.charges.foldLeft(self) { case (res, (chargeClass, time)) =>
        res.sumCharge(chargeClass, time)
      }
  }

  given CommutativeMonoid[CategorizedTime] =
    CommutativeMonoid.instance(Zero, _ +| _)

  /**
   * Order by the sum of charges, then by program then partner.
   */
  given Order[CategorizedTime] =
    Order.whenEqual[CategorizedTime](
      Order.by(pt => ChargeClass.values.toList.foldMap(pt.getOrZero)),  // Order.by(_.sum) picks up Map's sum :-/
      Order.whenEqual(
        Order.by(_.getOrZero(ChargeClass.Program)),
        Order.by(_.getOrZero(ChargeClass.Partner))
      )
    )

}
