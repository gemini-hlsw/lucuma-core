// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import cats.Order
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.{Validate => RefinedValidate}
import eu.timepit.refined.cats._
import eu.timepit.refined.refineV
import lucuma.core.optics.SplitEpi
import singleton.ops._

import scala.annotation.unused

/**
 * A wrapper around a Refined BigDecimal that is limited to a specified number of decimals places.
 *
 * The P type parameter is the refinement. The Dec type parameter must be a Singleton Int > 0 and <
 * 10, which is enforced by the compiler.
 *
 * @param value
 *   The refined BigDecimal. It is guaranteed to have a scale of no more than Dec.
 * @param req
 *   Requirement that 0 < Dec < 10
 * @param vo
 *   Evidence that Dec is a Singleton type.
 */
sealed abstract case class TruncatedRefinedBigDecimal[P, Dec <: XInt] private (
  value:                BigDecimal Refined P
)(implicit @unused req: Require[&&[Dec > 0, Dec < 10]], vo: ValueOf[Dec]) {
  val decimals: XInt = vo.value
}

object TruncatedRefinedBigDecimal {

  def apply[P, Dec <: XInt](value: BigDecimal Refined P)(implicit
    v:                             RefinedValidate[BigDecimal, P],
    req:                           Require[&&[Dec > 0, Dec < 10]],
    vo:                            ValueOf[Dec]
  ): Option[TruncatedRefinedBigDecimal[P, Dec]] = {
    val truncBD = value.value.setScale(vo.value, BigDecimal.RoundingMode.HALF_UP)
    refineV[P](truncBD).toOption.map(v => new TruncatedRefinedBigDecimal[P, Dec](v) {})
  }

  /**
   * Gets a SplitEpi for the TruncatedRefinedBigDecimal.
   *
   * This is unsafe under some occassions. For instance, say P is Interval.Closed[1.23, 7.28] and 1
   * decimal place is specified via Dec. If the input value to get() is 1.23, it will get rounded
   * down to 1.2, which is invalid and an exception is thrown. Since we are rounding up, an imput
   * value of 7.28 will be rounded up to 7.3, which is also invalid (using FLOOR would be safe for
   * the upper bound, but not the lower bound.).
   *
   * In general, you have to specify at least as many decimals for the rounding as the scale
   * implicitly specified in the Interval bounds. For this example, Dec would need to be set to at
   * least 2.
   *
   * Non Interval refinements can also be problematic, so use this with caution.
   */
  def unsafeRefinedBigDecimal[P, Dec <: XInt](implicit
    v:           RefinedValidate[BigDecimal, P],
    @unused req: Require[&&[Dec > 0, Dec < 10]],
    vo:          ValueOf[Dec]
  ): SplitEpi[BigDecimal Refined P, TruncatedRefinedBigDecimal[P, Dec]] =
    SplitEpi(TruncatedRefinedBigDecimal[P, Dec](_).get, _.value)

  implicit def truncatedRefinedBigDecimalOrder[P, Dec <: XInt]
    : Order[TruncatedRefinedBigDecimal[P, Dec]] =
    Order.by(_.value)
}
