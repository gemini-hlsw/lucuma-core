// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.util

import java.math.MathContext
import scala.math.BigDecimal.RoundingMode.HALF_UP
import scala.math.pow

object Percent {
  val DefaultPrecision = 2
  val Zero    = Percent(  0)
  val Hundred = Percent(100)

  /**
   * Create a percentage from a double representation.  For example, 57.7%
   * would be created via Percent(57.7).
   */
  def apply(d: Double): Percent = apply(d, DefaultPrecision)

  def apply(d: Double, scale: Int): Percent = Percent(BigDecimal(d).setScale(scale, HALF_UP))

  /**
   * Create from a quotient.  For example, 57.7% would be created via
   * Percent.fromQuotient(0.577).
   */
  def fromQuotient(d: Double): Percent = apply(d * 100.0)

  private val Mc = new MathContext(0, java.math.RoundingMode.HALF_UP)

  /**
   * Compute the relative percentages of the values in the provided list.  The
   * percentages will have the given precision and are normalized to sum to
   * exactly 100%.
   *
   * @param ts values whose percentages should be calculated
   * @param precision precision of the resulting Percentages (must be non-negative)
   * @param num values must be numeric
   *
   * @return List of Percentage where the i'th Percentage in the list
   *         corresponds to the i'th value in the list of ts
   */
  def relativePercentages[T](ts: List[T], precision: Int = DefaultPrecision)(implicit num: Numeric[T]): List[Percent] = {
    require(precision >= 0)

    val sum   = num.toDouble(ts.sum)
    val total = pow(10, 2.0 + precision).toInt

    def nonZeroPercents: List[Percent] = {

      // Percentages expressed as raw Doubles
      val percs = ts.map(t => num.toDouble(t) / sum * total)

      // List of percentages broken into triplets of
      // (whole number: Int, fraction: Double [0, 1), index: Int)
      val wholesFractionsIndices = percs.zipWithIndex.map {
        case (p, i) => (p.toInt, p - p.toInt, i)
      }

      val sumOfWholes = wholesFractionsIndices.map(_._1).sum

      // Count of values to round to the next highest whole number.
      val roundingCount = total - sumOfWholes

      // Sort by the fractional amount, decreasing.  That is, the largest
      // fractions come first.  Discard the fractional amount afterwords.
      val unscaledSorted = wholesFractionsIndices.sortBy(- _._2).map {
        case (w, _, i) => (w, i)
      }

      // "round" is the list of unscaled values to be incremented (i.e., rounded
      // up), "trunc" will be truncated (i.e., unchanged).
      val (round, trunc) = unscaledSorted.splitAt(roundingCount)

      // Round and then sort back to the original order.  These are the final
      // unscaled values.
      val normalized = (round.map { case (w, i) => (w + 1, i) } ++ trunc).sortBy(_._2).unzip._1

      normalized.map(w => Percent(BigDecimal(w.toLong, precision, Mc)))
    }

    if (sum == 0.0) List.fill(ts.size)(Percent.Zero) else nonZeroPercents
  }
}

final case class Percent(value: BigDecimal) extends Ordered[Percent] {
  private def quotient: BigDecimal = BigDecimal(value.underlying().movePointLeft(2))
  def *(thatValue: Int): Double    = (quotient * thatValue).doubleValue
  def *(thatValue: Long): Double   = (quotient * thatValue).doubleValue
  def *(thatValue: Double): Double = (quotient * thatValue).doubleValue

  def +(that: Percent): Percent = Percent(value + that.value)
  def -(that: Percent): Percent = Percent(value - that.value)

  override def toString: String = value.toString + "%"

  def compare(that: Percent): Int = value.compare(that.value)

  def doubleValue: Double = value.doubleValue
}