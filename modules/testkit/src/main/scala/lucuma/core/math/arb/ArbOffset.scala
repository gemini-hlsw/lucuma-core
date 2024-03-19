// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math.Angle
import lucuma.core.math.Axis
import lucuma.core.math.Offset
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen.*

trait ArbOffset {
  import ArbAngle.given

  given arbOffsetComponent[A]: Arbitrary[Offset.Component[A]] =
    Arbitrary(
      Gen
        .chooseNum(-10000, 10000)
        .map(mas => Offset.Component[A](Angle.milliarcseconds.reverseGet(mas)))
    )

  given arbAxisP: Arbitrary[Offset.Component[Axis.P]] = arbOffsetComponent[Axis.P]
  given arbAxisQ: Arbitrary[Offset.Component[Axis.Q]] = arbOffsetComponent[Axis.Q]

  given Arbitrary[Offset] =
    Arbitrary {
      for {
        p <- arbitrary[Offset.Component[Axis.P]]
        q <- arbitrary[Offset.Component[Axis.Q]]
      } yield Offset(p, q)
    }

  given cogOffsetComponent[A]: Cogen[Offset.Component[A]] =
    Cogen[Angle].contramap(_.toAngle)

  given cogenAxisP: Cogen[Offset.Component[Axis.P]] = cogOffsetComponent[Axis.P]
  given cogenAxisQ: Cogen[Offset.Component[Axis.Q]] = cogOffsetComponent[Axis.Q]

  given Cogen[Offset] =
    Cogen[(Offset.Component[Axis.P], Offset.Component[Axis.Q])].contramap(o => (o.p, o.q))

}

object ArbOffset extends ArbOffset
