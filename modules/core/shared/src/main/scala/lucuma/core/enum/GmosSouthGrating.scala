// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package `enum`

import cats.syntax.eq._
import coulomb.qopaque.{Quantity, withUnit}
import coulomb.policy.spire.standard.given
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.units.{*, given}
import lucuma.core.util.Enumerated
import spire.math.Rational

/**
 * Enumerated type for GMOS South gratings.
 * @group Enumerations (Generated)
 * @see https://www.gemini.edu/instrumentation/gmos/components#Gratings
 */
sealed abstract class GmosSouthGrating(
  val tag: String,
  val shortName: String,
  val longName: String,
  val rulingDensity: Int,
  val dispersion: Quantity[Rational, NanometersPerPixel],
  val obsolete: Boolean
) extends Product with Serializable

object GmosSouthGrating {

  private def pmToDispersion(pm: Int): Quantity[Rational, NanometersPerPixel] =
    PosInt.unsafeFrom(pm).withUnit[PicometersPerPixel].toValue[Rational].toUnit[NanometersPerPixel]

  /** @group Constructors */ case object B1200_G5321 extends GmosSouthGrating("B1200_G5321", "B1200", "B1200_G5321", 1200, pmToDispersion( 26), false)
  /** @group Constructors */ case object R831_G5322  extends GmosSouthGrating("R831_G5322",  "R831",  "R831_G5322",   831, pmToDispersion( 38), false)
  /** @group Constructors */ case object B600_G5323  extends GmosSouthGrating("B600_G5323",  "B600",  "B600_G5323",   600, pmToDispersion( 50), false)
  /** @group Constructors */ case object R600_G5324  extends GmosSouthGrating("R600_G5324",  "R600",  "R600_G5324",   600, pmToDispersion( 52), false)
  /** @group Constructors */ case object B480_G5327  extends GmosSouthGrating("B480_G5327",  "B480",  "B480_G5327",   480, pmToDispersion( 62), false)
  /** @group Constructors */ case object R400_G5325  extends GmosSouthGrating("R400_G5325",  "R400",  "R400_G5325",   400, pmToDispersion( 74), false)
  /** @group Constructors */ case object R150_G5326  extends GmosSouthGrating("R150_G5326",  "R150",  "R150_G5326",   150, pmToDispersion(193), false)

  /** All members of GmosSouthDisperser, in canonical order. */
  lazy val all: List[GmosSouthGrating] =
    List(B1200_G5321, R831_G5322, B600_G5323, R600_G5324, B480_G5327, R400_G5325, R150_G5326)

  /** Select the member of GmosSouthDisperser with the given tag, if any. */
  def fromTag(s: String): Option[GmosSouthGrating] =
    all.find(_.tag === s)

  /** Select the member of GmosSouthDisperser with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosSouthGrating =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosSouthDisperser: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosSouthGratingEnumerated: Enumerated[GmosSouthGrating] =
    new Enumerated[GmosSouthGrating] {
      def all: List[GmosSouthGrating] = GmosSouthGrating.all
      def tag(a: GmosSouthGrating): String = a.tag
      override def unsafeFromTag(s: String): GmosSouthGrating =
        GmosSouthGrating.unsafeFromTag(s)
    }

}
