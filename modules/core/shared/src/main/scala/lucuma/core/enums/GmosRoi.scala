// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS ROI (region of interest).
 * @group Enumerations (Generated)
 */
sealed abstract class GmosRoi(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object GmosRoi {

  /** @group Constructors */ case object FullFrame       extends GmosRoi("FullFrame", "full", "Full Frame Readout")
  /** @group Constructors */ case object Ccd2            extends GmosRoi("Ccd2", "ccd2", "CCD 2")
  /** @group Constructors */ case object CentralSpectrum extends GmosRoi("CentralSpectrum", "cspec", "Central Spectrum")
  /** @group Constructors */ case object CentralStamp    extends GmosRoi("CentralStamp", "stamp", "Central Stamp")
  /** @group Constructors */ case object Custom          extends GmosRoi("Custom", "custom", "Custom ROI")

  /** All members of GmosRoi, in canonical order. */
  val all: List[GmosRoi] =
    List(FullFrame, Ccd2, CentralSpectrum, CentralStamp, Custom)

  /** Select the member of GmosRoi with the given tag, if any. */
  def fromTag(s: String): Option[GmosRoi] =
    all.find(_.tag === s)

  /** Select the member of GmosRoi with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosRoi =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosRoi: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosRoiEnumerated: Enumerated[GmosRoi] =
    new Enumerated[GmosRoi] {
      def all = GmosRoi.all
      def tag(a: GmosRoi) = a.tag
      override def unsafeFromTag(s: String): GmosRoi =
        GmosRoi.unsafeFromTag(s)
    }

}
