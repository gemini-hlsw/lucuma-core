// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enum

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
 */
sealed abstract class GmosDetector(
  val tag: String,
  val shortName: String,
  val longName: String,
  val pixelSize: Angle,
  val shuffleOffset: Quantity[PosInt, Pixels],
  val xSize: Quantity[PosInt, Pixels],
  val ySize: Quantity[PosInt, Pixels],
  val maxRois: PosInt
) extends Product with Serializable {
}

object GmosDetector {

  /** @group Constructors */ case object GmosNE2V extends GmosDetector(
                               "GmosNE2V",
                               "E2V",
                               "E2V",
                               Angle.fromMicroarcseconds(72700),
                               1536.withRefinedUnit[Positive, Pixels],
                               6144.withRefinedUnit[Positive, Pixels],
                               4608.withRefinedUnit[Positive, Pixels],
                               4
                             )
  /** @group Constructors */ case object GmosSE2V extends GmosDetector(
                               "GmosSE2V",
                               "E2V",
                               "E2V",
                               Angle.fromMicroarcseconds(73000),
                               1536.withRefinedUnit[Positive, Pixels],
                               6144.withRefinedUnit[Positive, Pixels],
                               4608.withRefinedUnit[Positive, Pixels],
                               4
                             )
  /** @group Constructors */ case object GmosNHammamtsu extends GmosDetector(
                               "GmosNHAMAMATSU",
                               "Hamamatsu",
                               "Hamamatsu",
                               Angle.fromMicroarcseconds(80900),
                               1392.withRefinedUnit[Positive, Pixels],
                               6278.withRefinedUnit[Positive, Pixels],
                               4176.withRefinedUnit[Positive, Pixels],
                               5
                             )
  /** @group Constructors */ case object GmosSHammamtsu extends GmosDetector(
                               "GmosSHAMAMATSU",
                               "Hamamatsu",
                               "Hamamatsu",
                               Angle.fromMicroarcseconds(80000),
                               1392.withRefinedUnit[Positive, Pixels],
                               6255.withRefinedUnit[Positive, Pixels],
                               4176.withRefinedUnit[Positive, Pixels],
                               5
                             )

  /** All members of GmosDetector, in canonical order. */
  val all: List[GmosDetector] =
    List(GmosNE2V, GmosSE2V, GmosNHammamtsu, GmosSHammamtsu)

  /** Select the member of GmosDetector with the given tag, if any. */
  def fromTag(s: String): Option[GmosDetector] =
    all.find(_.tag === s)

  /** Select the member of GmosDetector with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosDetector =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosDetector: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosDetectorEnumerated: Enumerated[GmosDetector] =
    new Enumerated[GmosDetector] {
      def all = GmosDetector.all
      def tag(a: GmosDetector) = a.tag
      override def unsafeFromTag(s: String): GmosDetector =
        GmosDetector.unsafeFromTag(s)
    }

}
