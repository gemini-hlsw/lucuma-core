// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import cats.Show
import cats.kernel.CommutativeGroup
import cats.syntax.all.*
import lucuma.core.optics.SplitMono
import monocle.Focus
import monocle.Iso
import monocle.Lens

import java.lang.Math.cos
import java.lang.Math.sin

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

  /**
   * Angular separation between two offset positions.
   */
  def distance(o: Offset): Angle = {
    val µas = Angle.signedMicroarcseconds.get
    val pd  = µas(p.toAngle) - µas(o.p.toAngle)
    val qd  = µas(q.toAngle) - µas(o.q.toAngle)
    Angle.fromMicroarcseconds(Math.sqrt((pd*pd + qd*qd).toDouble).round)
  }

  /** This offset pair in radians. */
  def toSignedDoubleRadians: (Double, Double) =
    (p.toSignedDoubleRadians, q.toSignedDoubleRadians)

  /** This offset pair in decimal arcseconds. */
  def toSignedDecimalArcseconds: (BigDecimal, BigDecimal) =
    (p.toSignedDecimalArcseconds, q.toSignedDecimalArcseconds)

  /** String representation of this Offset, for debugging purposes only. */
  override def toString: String =
    s"Offset(Offset.P(${p.toAngle}), Offset.Q(${q.toAngle}))"
}

object Offset extends OffsetOptics {

  /** The zero offset. */
  val Zero: Offset =
    Offset(Component.Zero[Axis.P], Component.Zero[Axis.Q])

  /** Offset forms a commutative group. */
  given CommutativeGroup[Offset] =
    new CommutativeGroup[Offset] {
      val empty: Offset = Zero
      def combine(a: Offset, b: Offset): Offset = a + b
      def inverse(a: Offset): Offset = -a
    }

  given Show[Offset] =
    Show.fromToString

  /** Offsets are ordered by p, then q. */
  given Order[Offset] =
    Order.by(o => (o.p, o.q))

  def symmetric(a: Angle): Offset =
    Offset(Component(a), Component(a))

  /** Component of an angular offset. */
  opaque type Component[A] = Long

  object Component extends ComponentOptics {
    inline def apply[A](toAngle: Angle): Component[A] = toAngle.toMicroarcseconds

    extension[A](component: Component[A])
      inline def toAngle: Angle = Angle.fromMicroarcseconds(component)

      /** This component, reflected around the 0 .. 180° axis. Exact, invertable. */
      inline def unary_- : Component[A] =
        -toAngle.toMicroarcseconds

      /** Sum of this component and `o` of the same type. Exact. */
      inline def +(o: Component[A]): Component[A] =
        Angle.fromMicroarcseconds(toAngle.toMicroarcseconds + o).toMicroarcseconds

      /** Difference of this component and `o` of the same type. Exact. */
      inline def -(o: Component[A]): Component[A] =
        Angle.fromMicroarcseconds(toAngle.toMicroarcseconds - o).toMicroarcseconds

      /** This component in signed radians. */
      inline def toSignedDoubleRadians: Double =
        toAngle.toSignedDoubleRadians

      /** This component in signed arceseconds. */
      inline def toSignedDecimalArcseconds: BigDecimal =
        Angle.signedDecimalArcseconds.get(toAngle)

    /** The zero [A] component. */
    inline def Zero[A]: Component[A] =
      Angle.Angle0.toMicroarcseconds

    /** Component[A] forms a commutative group. */
    given commutativeGroup[A]: CommutativeGroup[Component[A]] =
      new CommutativeGroup[Component[A]] {
        val empty: Component[A] = Zero[A]
        def combine(a: Component[A], b: Component[A]): Component[A] = a + b
        def inverse(a: Component[A]): Component[A] = -a
      }

    given showComponent[A]: Show[Component[A]] =
      Show.fromToString

    /** Components are by signed angle. */
    given orderComponent[A]: Order[Component[A]] =
      Angle.SignedAngleOrder.contramap(_.toAngle)
  }

  trait ComponentOptics {
    import Component.*

    /** @group Optics */
    def angle[A]: Iso[Component[A], Angle] =
      Iso[Component[A], Angle](_.toAngle)(w => Component.apply[A](w))

    def signedDecimalArcseconds[A]: SplitMono[Component[A], BigDecimal] =
      Angle.signedDecimalArcseconds.imapA(Component.apply[A](_), _.toAngle)
  }

  // P, Q types and objects defined for convenience.

  type P = Component[Axis.P]
  type Q = Component[Axis.Q]

  protected trait ComponentCompanion[A] {

    val Zero: Component[A] = Component.Zero[A]

    val angle: Iso[Component[A], Angle] = Component.angle[A]

    val signedDecimalArcseconds: SplitMono[Component[A], BigDecimal] =
      Component.signedDecimalArcseconds[A]
  }

  object P extends ComponentCompanion[Axis.P] {
    def apply(toAngle: Angle): Component[Axis.P] = Component[Axis.P](toAngle)
  }

  object Q extends ComponentCompanion[Axis.Q] {
    def apply(toAngle: Angle): Component[Axis.Q] = Component[Axis.Q](toAngle)
  }

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
