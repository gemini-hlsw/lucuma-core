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
 * Enumerated type for GMOS South dispersers.
 * @group Enumerations (Generated)
 * @see https://www.gemini.edu/instrumentation/gmos/components#Gratings
 */
sealed abstract class GmosSouthDisperser(
  val tag: String,
  val shortName: String,
  val longName: String,
  val rulingDensity: Int,
  val dispersion: Quantity[Rational, NanometersPerPixel],
  val obsolete: Boolean
) extends Product with Serializable

object GmosSouthDisperser {

  private def pmToDispersion(pm: Int): Quantity[Rational, NanometersPerPixel] =
    PosInt.unsafeFrom(pm).withUnit[PicometersPerPixel].to[Rational, NanometersPerPixel]

  /** @group Constructors */ case object B1200_G5321 extends GmosSouthDisperser("B1200_G5321", "B1200", "B1200_G5321", 1200, pmToDispersion( 26), false)
  /** @group Constructors */ case object R831_G5322  extends GmosSouthDisperser("R831_G5322",  "R831",  "R831_G5322",   831, pmToDispersion( 38), false)
  /** @group Constructors */ case object B600_G5323  extends GmosSouthDisperser("B600_G5323",  "B600",  "B600_G5323",   600, pmToDispersion( 50), false)
  /** @group Constructors */ case object R600_G5324  extends GmosSouthDisperser("R600_G5324",  "R600",  "R600_G5324",   600, pmToDispersion( 52), false)
  /** @group Constructors */ case object B480_G5327  extends GmosSouthDisperser("B480_G5327",  "B480",  "B480_G5327",   480, pmToDispersion( 62), false)
  /** @group Constructors */ case object R400_G5325  extends GmosSouthDisperser("R400_G5325",  "R400",  "R400_G5325",   400, pmToDispersion( 74), false)
  /** @group Constructors */ case object R150_G5326  extends GmosSouthDisperser("R150_G5326",  "R150",  "R150_G5326",   150, pmToDispersion(193), false)

  /** All members of GmosSouthDisperser, in canonical order. */
  val all: List[GmosSouthDisperser] =
    List(B1200_G5321, R831_G5322, B600_G5323, R600_G5324, B480_G5327, R400_G5325, R150_G5326)

  /** Select the member of GmosSouthDisperser with the given tag, if any. */
  def fromTag(s: String): Option[GmosSouthDisperser] =
    all.find(_.tag === s)

  /** Select the member of GmosSouthDisperser with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosSouthDisperser =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosSouthDisperser: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosSouthDisperserEnumerated: Enumerated[GmosSouthDisperser] =
    new Enumerated[GmosSouthDisperser] {
      def all = GmosSouthDisperser.all
      def tag(a: GmosSouthDisperser) = a.tag
      override def unsafeFromTag(s: String): GmosSouthDisperser =
        GmosSouthDisperser.unsafeFromTag(s)
    }

}
