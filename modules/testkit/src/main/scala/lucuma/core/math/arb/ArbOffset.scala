// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math.Angle
import lucuma.core.math.Axis
import lucuma.core.math.Offset
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbOffset {
  import ArbAngle._

  implicit def arbOffsetComponent[A]: Arbitrary[Offset.Component[A]] =
    Arbitrary(
      Gen
        .chooseNum(-10000, 10000)
        .map(mas => Offset.Component[A](Angle.milliarcseconds.reverseGet(mas)))
    )

  implicit val arbOffsetP: Arbitrary[Offset.Component[Axis.P]] = arbOffsetComponent[Axis.P]
  implicit val arbOffsetQ: Arbitrary[Offset.Component[Axis.Q]] = arbOffsetComponent[Axis.Q]

  implicit val arbOffset: Arbitrary[Offset] =
    Arbitrary {
      for {
        p <- arbitrary[Offset.Component[Axis.P]]
        q <- arbitrary[Offset.Component[Axis.Q]]
      } yield Offset(p, q)
    }

  implicit def cogOffsetComponent[A]: Cogen[Offset.Component[A]] =
    Cogen[Angle].contramap(_.toAngle)

  implicit val cogOffsetP: Cogen[Offset.Component[Axis.P]] = cogOffsetComponent[Axis.P]
  implicit val cogOffsetQ: Cogen[Offset.Component[Axis.Q]] = cogOffsetComponent[Axis.Q]

  implicit val cogOffset: Cogen[Offset] =
    Cogen[(Offset.Component[Axis.P], Offset.Component[Axis.Q])].contramap(o => (o.p, o.q))

}

object ArbOffset extends ArbOffset
