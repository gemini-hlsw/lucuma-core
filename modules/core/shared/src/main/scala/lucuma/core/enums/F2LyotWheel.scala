// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Flamingos2 Lyot wheel.
 *
 * f/16:  plate scale = 1.61 arcsec/mm;  pixel scale=0.18 arcsec/pixel
 * f/32:  plate scale = 0.805 arcsec/mm; pixel scale =0.09 arcsec/pixel
 * If the Lyot wheel is set to HartmannA or HartmannB, the
 * FOV should just be a point at the base position (this is not a
 * scientifically useful option, but is used for focusing)
 * @group Enumerations (Generated)
 */
sealed abstract class F2LyotWheel(
  val tag: String,
  val shortName: String,
  val longName: String,
  val plateScale: Double, // arcsec/mm
  val pixelScale: Double, // arcsec/pixel
  val obsolete: Boolean
) extends Product with Serializable

object F2LyotWheel {

  /** @group Constructors */ case object F16 extends F2LyotWheel("F16", "f/16", "f/16 (Open)", 1.61, 0.18, false)
  /** @group Constructors */ case object F32High extends F2LyotWheel("F32High", "f/32 High", "f/32 MCAO high background", 0.805, 0.09, true)
  /** @group Constructors */ case object F32Low extends F2LyotWheel("F32Low", "f/32 Low", "f/32 MCAO low background", 0.805, 0.09, true)
  /** @group Constructors */ case object F33Gems extends F2LyotWheel("F33Gems", "f/33 GeMS", "f/33 (GeMS)", 0.784, 0.09, true)
  /** @group Constructors */ case object GemsUnder extends F2LyotWheel("GemsUnder", "GeMS Under", "f/33 (GeMS under-sized)", 0.784, 0.09, false)
  /** @group Constructors */ case object GemsOver extends F2LyotWheel("GemsOver", "GeMS Over", "f/33 (GeMS over-sized)", 0.784, 0.09, false)
  /** @group Constructors */ case object HartmannA extends F2LyotWheel("HartmannA", "Hartmann A (H1)", "Hartmann A (H1)", 0.0, 0.0, false)
  /** @group Constructors */ case object HartmannB extends F2LyotWheel("HartmannB", "Hartmann B (H2)", "Hartmann B (H2)", 0.0, 0.0, false)

  /** All members of F2LyotWheel, in canonical order. */
  val all: List[F2LyotWheel] =
    List(F16, F32High, F32Low, F33Gems, GemsUnder, GemsOver, HartmannA, HartmannB)

  /** Select the member of F2LyotWheel with the given tag, if any. */
  def fromTag(s: String): Option[F2LyotWheel] =
    all.find(_.tag === s)

  /** Select the member of F2LyotWheel with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): F2LyotWheel =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"F2LyotWheel: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  given Enumerated[F2LyotWheel] =
    new Enumerated[F2LyotWheel] {
      def all = F2LyotWheel.all
      def tag(a: F2LyotWheel) = a.tag
      override def unsafeFromTag(s: String): F2LyotWheel =
        F2LyotWheel.unsafeFromTag(s)
    }

}
