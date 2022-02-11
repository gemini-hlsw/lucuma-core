// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package `enum`

import cats.syntax.eq._
import coulomb._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.units._
import lucuma.core.util.Enumerated
import spire.math.Rational

/**
 * Enumerated type for GMOS North dispersers.
 * @group Enumerations (Generated)
 * @see https://www.gemini.edu/instrumentation/gmos/components#Gratings
 */
sealed abstract class GmosNorthDisperser(
  val tag: String,
  val shortName: String,
  val longName: String,
  val rulingDensity: Int,
  val dispersion: Quantity[Rational, NanometersPerPixel],
  val obsolete: Boolean
) extends Product with Serializable

object GmosNorthDisperser {

  private def pmToDispersion(pm: Int): Quantity[Rational, NanometersPerPixel] =
    PosInt.unsafeFrom(pm).withUnit[PicometersPerPixel].to[Rational, NanometersPerPixel]

  /** @group Constructors */ case object B1200_G5301 extends GmosNorthDisperser("B1200_G5301", "B1200", "B1200_G5301", 1200, pmToDispersion( 26), false)
  /** @group Constructors */ case object R831_G5302  extends GmosNorthDisperser("R831_G5302",  "R831",  "R831_G5302",   831, pmToDispersion( 38), false)
  /** @group Constructors */ case object B600_G5303  extends GmosNorthDisperser("B600_G5303",  "B600",  "B600_G5303",   600, pmToDispersion( 45), true)
  /** @group Constructors */ case object B600_G5307  extends GmosNorthDisperser("B600_G5307",  "B600",  "B600_G5307",   600, pmToDispersion( 50), false)
  /** @group Constructors */ case object R600_G5304  extends GmosNorthDisperser("R600_G5304",  "R600",  "R600_G5304",   600, pmToDispersion( 52), false)
  /** @group Constructors */ case object B480_G5309  extends GmosNorthDisperser("B480_G5309",  "B480",  "B480_G5309",   480, pmToDispersion( 62), false)
  /** @group Constructors */ case object R400_G5305  extends GmosNorthDisperser("R400_G5305",  "R400",  "R400_G5305",   400, pmToDispersion( 74), false)
  /** @group Constructors */ case object R150_G5306  extends GmosNorthDisperser("R150_G5306",  "R150",  "R150_G5306",   150, pmToDispersion(174), true)
  /** @group Constructors */ case object R150_G5308  extends GmosNorthDisperser("R150_G5308",  "R150",  "R150_G5308",   150, pmToDispersion(193), false)

  /** All members of GmosNorthDisperser, in canonical order. */
  val all: List[GmosNorthDisperser] =
    List(B1200_G5301, R831_G5302, B600_G5303, B600_G5307, R600_G5304, B480_G5309, R400_G5305, R150_G5306, R150_G5308)

  /** Select the member of GmosNorthDisperser with the given tag, if any. */
  def fromTag(s: String): Option[GmosNorthDisperser] =
    all.find(_.tag === s)

  /** Select the member of GmosNorthDisperser with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosNorthDisperser =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosNorthDisperser: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosNorthDisperserEnumerated: Enumerated[GmosNorthDisperser] =
    new Enumerated[GmosNorthDisperser] {
      def all = GmosNorthDisperser.all
      def tag(a: GmosNorthDisperser) = a.tag
      override def unsafeFromTag(s: String): GmosNorthDisperser =
        GmosNorthDisperser.unsafeFromTag(s)
    }

}
