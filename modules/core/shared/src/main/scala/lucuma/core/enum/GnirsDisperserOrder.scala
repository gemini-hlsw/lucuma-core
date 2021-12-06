// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enum
import cats.syntax.eq._
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GNIRS Disperser Order.
 * @group Enumerations
 */
sealed abstract class GnirsDisperserOrder(
  val tag:               String,
  val shortName:         String,
  val longName:          String,
  val count:             Int,
  val defaultWavelength: Wavelength,
  val minWavelength:     Wavelength,
  val maxWavelength:     Wavelength,
  val deltaWavelength:   Wavelength,
  val band:              Option[Band],
  val crossDispersed:    Boolean
) extends Product
    with Serializable

object GnirsDisperserOrder {

  /** @group Constructors */
  case object One
      extends GnirsDisperserOrder(
        "One",
        "1",
        "One",
        1,
        Wavelength.unsafeFromInt(4850000),
        Wavelength.unsafeFromInt(4300000),
        Wavelength.unsafeFromInt(6000000),
        Wavelength.unsafeFromInt(0),
        Some(Band.M),
        false
      )

  /** @group Constructors */
  case object Two
      extends GnirsDisperserOrder(
        "Two",
        "2",
        "Two",
        2,
        Wavelength.unsafeFromInt(3400000),
        Wavelength.unsafeFromInt(2700000),
        Wavelength.unsafeFromInt(4300000),
        Wavelength.unsafeFromInt(0),
        Some(Band.L),
        false
      )

  /** @group Constructors */
  case object Three
      extends GnirsDisperserOrder(
        "Three",
        "3",
        "Three",
        3,
        Wavelength.unsafeFromInt(2220000),
        Wavelength.unsafeFromInt(1860000),
        Wavelength.unsafeFromInt(2700000),
        Wavelength.unsafeFromInt(647),
        Some(Band.K),
        true
      )

  /** @group Constructors */
  case object FourXD
      extends GnirsDisperserOrder(
        "FourXD",
        "4XD",
        "FourXD",
        4,
        Wavelength.unsafeFromInt(1650000),
        Wavelength.unsafeFromInt(1420000),
        Wavelength.unsafeFromInt(1860000),
        Wavelength.unsafeFromInt(482),
        Some(Band.H),
        true
      )

  /** @group Constructors */
  case object Four
      extends GnirsDisperserOrder(
        "Four",
        "4",
        "Four",
        4,
        Wavelength.unsafeFromInt(1630000),
        Wavelength.unsafeFromInt(1420000),
        Wavelength.unsafeFromInt(1860000),
        Wavelength.unsafeFromInt(485),
        Some(Band.H),
        true
      )

  /** @group Constructors */
  case object Five
      extends GnirsDisperserOrder(
        "Five",
        "5",
        "Five",
        5,
        Wavelength.unsafeFromInt(1250000),
        Wavelength.unsafeFromInt(1170000),
        Wavelength.unsafeFromInt(1420000),
        Wavelength.unsafeFromInt(388),
        Some(Band.J),
        true
      )

  /** @group Constructors */
  case object Six
      extends GnirsDisperserOrder(
        "Six",
        "6",
        "Six",
        6,
        Wavelength.unsafeFromInt(1100000),
        Wavelength.unsafeFromInt(1030000),
        Wavelength.unsafeFromInt(1170000),
        Wavelength.unsafeFromInt(323),
        None,
        true
      )

  /** @group Constructors */
  case object Seven
      extends GnirsDisperserOrder(
        "Seven",
        "7",
        "Seven",
        7,
        Wavelength.unsafeFromInt(951000),
        Wavelength.unsafeFromInt(880000),
        Wavelength.unsafeFromInt(1030000),
        Wavelength.unsafeFromInt(276),
        None,
        true
      )

  /** @group Constructors */
  case object Eight
      extends GnirsDisperserOrder(
        "Eight",
        "8",
        "Eight",
        8,
        Wavelength.unsafeFromInt(832000),
        Wavelength.unsafeFromInt(780000),
        Wavelength.unsafeFromInt(880000),
        Wavelength.unsafeFromInt(241),
        None,
        true
      )

  /** All members of GnirsDisperserOrder, in canonical order. */
  val all: List[GnirsDisperserOrder] =
    List(One, Two, Three, FourXD, Four, Five, Six, Seven, Eight)

  /** Select the member of GnirsDisperserOrder with the given tag, if any. */
  def fromTag(s: String): Option[GnirsDisperserOrder] =
    all.find(_.tag === s)

  /** Select the member of GnirsDisperserOrder with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GnirsDisperserOrder =
    fromTag(s).getOrElse(
      throw new NoSuchElementException(s"GnirsDisperserOrder: Invalid tag: '$s'")
    )

  /** @group Typeclass Instances */
  implicit val GnirsDisperserOrderEnumerated: Enumerated[GnirsDisperserOrder] =
    new Enumerated[GnirsDisperserOrder] {
      def all                                                    = GnirsDisperserOrder.all
      def tag(a: GnirsDisperserOrder)                            = a.tag
      override def unsafeFromTag(s: String): GnirsDisperserOrder =
        GnirsDisperserOrder.unsafeFromTag(s)
    }

}
