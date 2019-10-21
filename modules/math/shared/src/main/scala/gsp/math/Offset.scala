// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.{ Order, Show }
import cats.kernel.CommutativeGroup
import cats.implicits._
import gsp.math.optics.SplitMono
import monocle.{ Iso, Lens }
import monocle.macros.{ GenIso, GenLens }

object Axis {
  type P
  type Q
}

/** Angular offset with P and Q components. */
final case class Offset(p: Offset.Component[Axis.P], q: Offset.Component[Axis.Q]) {
  /** This offset, with both components reflected around the 0 .. 180° axis. Exact, invertable. */
  def unary_- : Offset =
    Offset(-p, -q)

  /** Componentwise sum of this offset and `o`. Exact. */
  def +(o: Offset): Offset =
    Offset(p + o.p, q + o.q)

  /** This offset pair in radians. */
  def toRadians: (Double, Double) =
    (p.toRadians, q.toRadians)

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

    /** This component in signed radians. */
    def toRadians: Double =
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

    def signedArcseconds[A]: SplitMono[Component[A], BigDecimal] =
      Angle.signedArcseconds.imapA(Component[A], _.toAngle)
  }

  // P, Q types and objects defined for convenience.

  type P = Component[Axis.P]
  type Q = Component[Axis.Q]

  protected trait ComponentCompanion[A] {
    def apply(toAngle: Angle): Component[A] = Component[A](toAngle)

    val Zero: Component[A] = Component.Zero[A]

    val angle: Iso[Component[A], Angle] = Component.angle[A]

    val signedArcseconds: SplitMono[Component[A], BigDecimal] = Component.signedArcseconds[A]
  }

  object P extends ComponentCompanion[Axis.P]

  object Q extends ComponentCompanion[Axis.Q]
}

trait OffsetOptics {

  /** @group Optics */
  val p: Lens[Offset, Offset.Component[Axis.P]] =
    GenLens[Offset](_.p)

  /** @group Optics */
  val q: Lens[Offset, Offset.Component[Axis.Q]] =
    GenLens[Offset](_.q)

  /** @group Optics */
  val pAngle: Lens[Offset, Angle] =
    p composeIso Offset.Component.angle[Axis.P]

  /** @group Optics */
  val qAngle: Lens[Offset, Angle] =
    q composeIso Offset.Component.angle[Axis.Q]

}