// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enum
import cats.syntax.eq._
import lucuma.core.math.Angle
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS South focal plane units.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosSouthFpu(
  val tag: String,
  val shortName: String,
  val longName: String,
  val slitWidth: Option[Angle],
  val xOffset: Angle,
  val obsolete: Boolean
) extends Product with Serializable

object GmosSouthFpu {

  /** @group Constructors */ case object Bhros extends GmosSouthFpu("Bhros", "bHROS", "bHROS", Option.empty[Angle], Angle.fromDoubleArcseconds(0.000), true)
  /** @group Constructors */ case object Ns1 extends GmosSouthFpu("Ns1", "NS0.5\"", "N and S 0.50 arcsec", Some(Angle.fromDoubleArcseconds(0.50)), Angle.fromDoubleArcseconds(0.000), false)
  /** @group Constructors */ case object Ns2 extends GmosSouthFpu("Ns2", "NS0.75\"", "N and S 0.75 arcsec", Some(Angle.fromDoubleArcseconds(0.75)), Angle.fromDoubleArcseconds(0.000), false)
  /** @group Constructors */ case object Ns3 extends GmosSouthFpu("Ns3", "NS1.0\"", "N and S 1.00 arcsec", Some(Angle.fromDoubleArcseconds(1.00)), Angle.fromDoubleArcseconds(0.000), false)
  /** @group Constructors */ case object Ns4 extends GmosSouthFpu("Ns4", "NS1.5\"", "N and S 1.50 arcsec", Some(Angle.fromDoubleArcseconds(1.50)), Angle.fromDoubleArcseconds(0.000), false)
  /** @group Constructors */ case object Ns5 extends GmosSouthFpu("Ns5", "NS2.0\"", "N and S 2.00 arcsec", Some(Angle.fromDoubleArcseconds(2.00)), Angle.fromDoubleArcseconds(0.000), false)
  /** @group Constructors */ case object LongSlit_0_25 extends GmosSouthFpu("LongSlit_0_25", "0.25\"", "Longslit 0.25 arcsec", Some(Angle.fromDoubleArcseconds(0.25)), Angle.fromDoubleArcseconds(0.000), false)
  /** @group Constructors */ case object LongSlit_0_50 extends GmosSouthFpu("LongSlit_0_50", "0.50\"", "Longslit 0.50 arcsec", Some(Angle.fromDoubleArcseconds(0.50)), Angle.fromDoubleArcseconds(0.000), false)
  /** @group Constructors */ case object LongSlit_0_75 extends GmosSouthFpu("LongSlit_0_75", "0.75\"", "Longslit 0.75 arcsec", Some(Angle.fromDoubleArcseconds(0.75)), Angle.fromDoubleArcseconds(0.000), false)
  /** @group Constructors */ case object LongSlit_1_00 extends GmosSouthFpu("LongSlit_1_00", "1.0\"", "Longslit 1.00 arcsec", Some(Angle.fromDoubleArcseconds(1.00)), Angle.fromDoubleArcseconds(0.000), false)
  /** @group Constructors */ case object LongSlit_1_50 extends GmosSouthFpu("LongSlit_1_50", "1.5\"", "Longslit 1.50 arcsec", Some(Angle.fromDoubleArcseconds(1.50)), Angle.fromDoubleArcseconds(0.000), false)
  /** @group Constructors */ case object LongSlit_2_00 extends GmosSouthFpu("LongSlit_2_00", "2.0\"", "Longslit 2.00 arcsec", Some(Angle.fromDoubleArcseconds(2.00)), Angle.fromDoubleArcseconds(0.000), false)
  /** @group Constructors */ case object LongSlit_5_00 extends GmosSouthFpu("LongSlit_5_00", "5.0\"", "Longslit 5.00 arcsec", Some(Angle.fromDoubleArcseconds(5.00)), Angle.fromDoubleArcseconds(0.000), false)
  /** @group Constructors */ case object Ifu2Slits extends GmosSouthFpu("Ifu2Slits", "IFU-2", "IFU 2 Slits", Option.empty[Angle], Angle.fromDoubleArcseconds(-31.750), false)
  /** @group Constructors */ case object IfuBlue extends GmosSouthFpu("IfuBlue", "IFU-B", "IFU Left Slit (blue)", Option.empty[Angle], Angle.fromDoubleArcseconds(-30.875), false)
  /** @group Constructors */ case object IfuRed extends GmosSouthFpu("IfuRed", "IFU-R", "IFU Right Slit (red)", Option.empty[Angle], Angle.fromDoubleArcseconds(-32.625), false)
  /** @group Constructors */ case object IfuNS2Slits extends GmosSouthFpu("IfuNS2Slits", "IFU-NS-2", "IFU N and S 2 Slits", Option.empty[Angle], Angle.fromDoubleArcseconds(-31.750), false)
  /** @group Constructors */ case object IfuNSBlue extends GmosSouthFpu("IfuNSBlue", "IFU-NS-B", "IFU N and S Left Slit (blue)", Option.empty[Angle], Angle.fromDoubleArcseconds(-30.875), false)
  /** @group Constructors */ case object IfuNSRed extends GmosSouthFpu("IfuNSRed", "IFU-NS-R", "IFU N and S Right Slit (red)", Option.empty[Angle], Angle.fromDoubleArcseconds(-32.625), false)

  /** All members of GmosSouthFpu, in canonical order. */
  val all: List[GmosSouthFpu] =
    List(Bhros, Ns1, Ns2, Ns3, Ns4, Ns5, LongSlit_0_25, LongSlit_0_50, LongSlit_0_75, LongSlit_1_00, LongSlit_1_50, LongSlit_2_00, LongSlit_5_00, Ifu2Slits, IfuBlue, IfuRed, IfuNS2Slits, IfuNSBlue, IfuNSRed)

  /** Select the member of GmosSouthFpu with the given tag, if any. */
  def fromTag(s: String): Option[GmosSouthFpu] =
    all.find(_.tag === s)

  /** Select the member of GmosSouthFpu with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosSouthFpu =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosSouthFpu: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosSouthFpuEnumerated: Enumerated[GmosSouthFpu] =
    new Enumerated[GmosSouthFpu] {
      def all = GmosSouthFpu.all
      def tag(a: GmosSouthFpu) = a.tag
      override def unsafeFromTag(s: String): GmosSouthFpu =
        GmosSouthFpu.unsafeFromTag(s)
    }

}
