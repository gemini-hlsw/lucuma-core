// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package `enum`

import cats.syntax.eq._
import coulomb._
import coulomb.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Angle
import lucuma.core.math.units.Pixels
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS detector.
 * @group Enumerations (Generated)
 * @see https://www.gemini.edu/instrumentation/gmos/components#Detector
 */
sealed abstract class GmosNorthDetector(
  val tag: String,
  val shortName: String,
  val longName: String,
  val pixelSize: Angle,
  val shuffleOffset: Quantity[PosInt, Pixels],
  val xSize: Quantity[PosInt, Pixels],
  val ySize: Quantity[PosInt, Pixels],
  val gapSize: Quantity[PosInt, Pixels],
  val maxRois: PosInt
) extends Product with Serializable {
}

object GmosNorthDetector {

  /** @group Constructors */ case object E2V extends GmosNorthDetector(
                               "E2V",
                               "E2V",
                               "E2V",
                               Angle.fromMicroarcseconds(72700),
                               1536.withRefinedUnit[Positive, Pixels],
                               6144.withRefinedUnit[Positive, Pixels],
                               4608.withRefinedUnit[Positive, Pixels],
                               37.withRefinedUnit[Positive, Pixels],
                               4
                             )
  /** @group Constructors */ case object Hamamatsu extends GmosNorthDetector(
                               "HAMAMATSU",
                               "Hamamatsu",
                               "Hamamatsu",
                               Angle.fromMicroarcseconds(80900),
                               1392.withRefinedUnit[Positive, Pixels],
                               6278.withRefinedUnit[Positive, Pixels],
                               4176.withRefinedUnit[Positive, Pixels],
                               80.withRefinedUnit[Positive, Pixels],
                               5
                             )

  /** All members of GmosNorthDetector, in canonical order. */
  val all: List[GmosNorthDetector] =
    List(E2V, Hamamatsu)

  /** Select the member of GmosNorthDetector with the given tag, if any. */
  def fromTag(s: String): Option[GmosNorthDetector] =
    all.find(_.tag === s)

  /** Select the member of GmosNorthDetector with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosNorthDetector =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosNorthDetector: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosDetectorEnumerated: Enumerated[GmosNorthDetector] =
    new Enumerated[GmosNorthDetector] {
      def all: List[GmosNorthDetector] =
        GmosNorthDetector.all

      def tag(a: GmosNorthDetector): String =
        a.tag

      override def unsafeFromTag(s: String): GmosNorthDetector =
        GmosNorthDetector.unsafeFromTag(s)
    }

}
