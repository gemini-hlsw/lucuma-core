// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Monoid
import cats.Order
import lucuma.core.util.TimeSpan

import scala.annotation.targetName

/**
 * Time required to produce an individual dataset.  This consists of the
 * exposure itself, followed by detector readout and then write to permanent store.
 */
case class DatasetEstimate(
  exposure: TimeSpan,
  readout:  TimeSpan,
  write:    TimeSpan
) {

  /**
   * Sum of the exposure, readout, and write time.
   */
  def estimate: TimeSpan =
    exposure +| readout +| write

  /**
   * Adds two DatasetEstimates by combining the two exposure times, the two
   * readouts, and the two writes.
   */
  @targetName("boundedAdd")
  def +|(other: DatasetEstimate): DatasetEstimate =
    DatasetEstimate(
      exposure +| other.exposure,
      readout  +| other.readout,
      write    +| other.write
    )
}

object DatasetEstimate {

  val Zero: DatasetEstimate =
    DatasetEstimate(TimeSpan.Zero, TimeSpan.Zero, TimeSpan.Zero)

  /**
   * Order primarily  by the total time estimate.
   */
  given Order[DatasetEstimate] =
    Order.by { e => (
      e.estimate,
      e.exposure,
      e.readout,
      e.write
    )}

  given Monoid[DatasetEstimate] =
    Monoid.instance(Zero, _ +| _)

}
