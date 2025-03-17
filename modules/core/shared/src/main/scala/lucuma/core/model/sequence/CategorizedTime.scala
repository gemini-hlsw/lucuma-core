// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import cats.implicits.catsKernelOrderingForOrder
import cats.kernel.CommutativeMonoid
import cats.syntax.foldable.*
import cats.syntax.functor.*
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ObserveClass
import lucuma.core.util.TimeSpan

import scala.collection.immutable.SortedMap

/**
 * Time charge broken down by charge class.
 */
case class CategorizedTime private (
  programTime: TimeSpan,
  nonCharged:  TimeSpan
):

  /**
   * Gets the time charged for the given charge class.
   */
  def apply(chargeClass: ChargeClass): TimeSpan =
    chargeClass match
      case ChargeClass.Program    => programTime
      case ChargeClass.NonCharged => nonCharged

  /**
   * Lists the charge classes and their time value.
   */
  def charges: List[(ChargeClass, TimeSpan)] =
    ChargeClass.values.toList.fproduct(apply)

  /**
   * Returns `true` if there are no charges for any charge class.
   */
  def isZero: Boolean =
    charges.forall((_, t) => t.isZero)

  /**
   * Returns `true` if there is a charge for at least one charge class.
   */
  def nonZero: Boolean =
    !isZero

  /**
   * Sets the value associated with the given `ChargeClass` to `time`.
   */
  def updated(chargeClass: ChargeClass, time: TimeSpan): CategorizedTime =
    chargeClass match
      case ChargeClass.Program    => copy(programTime = time)
      case ChargeClass.NonCharged => copy(nonCharged  = time)

  /**
   * Modifies the amount associated with the given charge class using the
   * provided operation.
   */
  def modify(chargeClass: ChargeClass, op: TimeSpan => TimeSpan): CategorizedTime =
    updated(chargeClass, op(apply(chargeClass)))

  /**
   * Adjusts this `CategorizedTime` instance according to the supplied
   * correction, adding or subtracting time associated its charge class but
   * bounded by the min and max `TimeSpan` values.
   */
  def correct(c: TimeChargeCorrection): CategorizedTime =
    c.op match
      case TimeChargeCorrection.Op.Add      => modify(c.chargeClass, _ +| c.amount)
      case TimeChargeCorrection.Op.Subtract => modify(c.chargeClass, _ -| c.amount)

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
    ChargeClass.values.toList.foldMap(apply)

  /**
   * Adds the corresponding charges for two CategorizedTime values.
   */
  def +|(other: CategorizedTime): CategorizedTime =
    other.charges.foldLeft(this) { case (res, (chargeClass, time)) =>
      res.sumCharge(chargeClass, time)
    }

object CategorizedTime:

  /**
   * A CategorizedTime instance which associates Zero time with all charge
   * classes.
   */
  val Zero: CategorizedTime =
    CategorizedTime(TimeSpan.Zero, TimeSpan.Zero)

  /**
   * Constructs a CategorizedTime instance from the provided charges.  If a
   * ChargeClass is duplicated in the argument list, the last instance applies.
   */
  def apply(charges: (ChargeClass, TimeSpan)*): CategorizedTime =
    from(charges)

  /**
   * Constructs a CategorizedTime instance from the provided charges.  If a
   * ChargeClass is duplicated in the argument list, the last instance applies.
   */
  def from(it: IterableOnce[(ChargeClass, TimeSpan)]): CategorizedTime =
    val m = SortedMap.from(it.iterator.filter { (_, t) => t.nonZero }).withDefaultValue(TimeSpan.Zero)
    CategorizedTime(m(ChargeClass.Program), m(ChargeClass.NonCharged))

  /**
   * Creates a `CategorizedTime` instance where the entirety of the step estimate
   * is associated with the `ObserveClass`'s `ChargeClass`.
   */
  def fromStep(observeClass: ObserveClass, stepEstimate: StepEstimate): CategorizedTime =
    apply(observeClass.chargeClass -> stepEstimate.total)

  given CommutativeMonoid[CategorizedTime] =
    CommutativeMonoid.instance(Zero, _ +| _)

  /**
   * Order by the sum of charges, then by program.
   */
  given Order[CategorizedTime] =
    Order.whenEqual[CategorizedTime](
      Order.by(_.sum),
      Order.by(_.apply(ChargeClass.Program))
    )