// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import cats.Show
import cats.kernel.CommutativeGroup
import cats.syntax.all._
import lucuma.core.optics.SplitMono
import monocle.Focus
import monocle.Iso
import monocle.Lens
import monocle.macros.GenIso

import scala.math.cos
import scala.math.sin

object Axis {
  type P
  type Q
}

/** Angular offset with P and Q components. */
final case class Offset(p: Offset.Component[Axis.P], q: Offset.Component[Axis.Q]) {

  /** This offset, with both components reflected around the 0 .. 180° axis. Exact, invertable. */
  def unary_- : Offset =
    Offset(-p, -q)

  /** Component-wise sum of this offset and `o`. Exact. */
  def +(o: Offset): Offset =
    Offset(p + o.p, q + o.q)

  /**
   * Component-wise subtraction of this offset and `o`. Exact.
   *
   * `this - o === this + -o`
   */
  def -(o: Offset): Offset =
    Offset(p - o.p, q - o.q)

  /**
   * Rotates the offset around the origin (counterclockwise) and produces a
   * new `Offset` at the resulting location. Approximate, non-invertible.
   *
   * @param θ rotation angle
   *
   * @return Offset at the new position
   */
  def rotate(θ: Angle): Offset =
    Offset.rotateBy(θ)(this)

  /** This offset pair in radians. */
  def toSignedDoubleRadians: (Double, Double) =
    (p.toSignedDoubleRadians, q.toSignedDoubleRadians)

  /** String representation of this Offset, for debugging purposes only. */
  override def toString: String =
    s"Offset(Offset.P(${p.toAngle}), Offset.Q(${q.toAngle}))"
}

object Offset extends OffsetOptics {

  /** The zero offset. */
  val Zero: Offset =
    Offset(Component.Zero[Axis.P], Component.Zero[Axis.Q])

  /** Offset forms a commutative group. */
  implicit val CommutativeGroupOffset: CommutativeGroup[Offset] =
    new CommutativeGroup[Offset] {
      val empty: Offset = Zero
      def combine(a: Offset, b: Offset): Offset = a + b
      def inverse(a: Offset): Offset = -a
    }

  implicit val ShowOffset: Show[Offset] =
    Show.fromToString

  /** Offsets are ordered by p, then q. */
  implicit val OrderOffset: Order[Offset] =
    Order.by(o => (o.p, o.q))

  /** Component of an angular offset. */
  final case class Component[A](toAngle: Angle) {

    /** This component, reflected around the 0 .. 180° axis. Exact, invertable. */
    def unary_- : Component[A] =
      Component[A](-toAngle)

    /** Sum of this component and `o` of the same type. Exact. */
    def +(o: Component[A]): Component[A] =
      Component[A](toAngle + o.toAngle)

    /** Difference of this component and `o` of the same type. Exact. */
    def -(o: Component[A]): Component[A] =
      Component[A](toAngle - o.toAngle)

    /** This component in signed radians. */
    def toSignedDoubleRadians: Double =
      toAngle.toSignedDoubleRadians
  }

  object Component extends ComponentOptics {

    /** The zero [A] component. */
    def Zero[A]: Component[A] =
      Component[A](Angle.Angle0)

    /** Component[A] forms a commutative group. */
    implicit def CommutativeGroupComponent[A]: CommutativeGroup[Component[A]] =
      new CommutativeGroup[Component[A]] {
        val empty: Component[A] = Zero[A]
        def combine(a: Component[A], b: Component[A]): Component[A] = a + b
        def inverse(a: Component[A]): Component[A] = -a
      }

    implicit def ShowComponent[A]: Show[Component[A]] =
      Show.fromToString

    /** Components are by signed angle. */
    implicit def OrderComponent[A]: Order[Component[A]] =
      Angle.SignedAngleOrder.contramap(_.toAngle)
  }

  trait ComponentOptics {

    /** @group Optics */
    def angle[A]: Iso[Component[A], Angle] =
      GenIso[Component[A], Angle]

    def signedDecimalArcseconds[A]: SplitMono[Component[A], BigDecimal] =
      Angle.signedDecimalArcseconds.imapA(Component[A], _.toAngle)
  }

  // P, Q types and objects defined for convenience.

  type P = Component[Axis.P]
  type Q = Component[Axis.Q]

  protected trait ComponentCompanion[A] {
    def apply(toAngle: Angle): Component[A] = Component[A](toAngle)

    val Zero: Component[A] = Component.Zero[A]

    val angle: Iso[Component[A], Angle] = Component.angle[A]

    val signedDecimalArcseconds: SplitMono[Component[A], BigDecimal] =
      Component.signedDecimalArcseconds[A]
  }

  object P extends ComponentCompanion[Axis.P]

  object Q extends ComponentCompanion[Axis.Q]

  /**
   * Produces a function that will calculate `Offset` positions rotated by
   * the given angle θ (counterclockwise).  Approximate, non-invertible.
   *
   * @param θ rotation angle
   */
  def rotateBy(θ: Angle): Offset => Offset = {
    val r = θ.toSignedDoubleRadians
    Offset.signedMicroarcseconds.modify {
      case (pµ, qµ) =>
        (
          // p is converted to normal cartesian coordinates and back here
          -(-pµ * cos(r) - qµ * sin(r)).round,
          (-pµ * sin(r) + qµ * cos(r)).round
        )
    }
  }

}

trait OffsetOptics {

  /** @group Optics */
  val p: Lens[Offset, Offset.Component[Axis.P]] =
    Focus[Offset](_.p)

  /** @group Optics */
  val q: Lens[Offset, Offset.Component[Axis.Q]] =
    Focus[Offset](_.q)

  /** @group Optics */
  val pAngle: Lens[Offset, Angle] =
    p.andThen(Offset.Component.angle[Axis.P])

  /** @group Optics */
  val qAngle: Lens[Offset, Angle] =
    q.andThen(Offset.Component.angle[Axis.Q])

  private def splitMonoFromAngleSplitMono[A](m: SplitMono[Angle, A]): SplitMono[Offset, (A, A)] =
    SplitMono(
      o => (m.get(o.p.toAngle), m.get(o.q.toAngle)),
      t => Offset(m.reverseGet(t._1).p, m.reverseGet(t._2).q)
    )

  /** @group Optics */
  val microarcseconds: SplitMono[Offset, (Long, Long)] =
    splitMonoFromAngleSplitMono(Angle.microarcseconds)

  /** @group Optics */
  val signedMicroarcseconds: SplitMono[Offset, (Long, Long)] =
    splitMonoFromAngleSplitMono(Angle.signedMicroarcseconds)

  /** @group Optics */
  val signedDecimalArcseconds: SplitMono[Offset, (BigDecimal, BigDecimal)] =
    splitMonoFromAngleSplitMono(Angle.signedDecimalArcseconds)

}
