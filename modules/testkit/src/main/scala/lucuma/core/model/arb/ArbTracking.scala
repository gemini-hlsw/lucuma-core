// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.math.*
import lucuma.core.math.arb.*
import lucuma.core.model.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen.*

trait ArbTracking {
  import ArbEphemeris.given
  import ArbSiderealTracking.given
  import ArbCoordinates.given

  given Arbitrary[ConstantTracking] = 
    Arbitrary:
      arbitrary[Coordinates].map:
        ConstantTracking(_)

  given Cogen[ConstantTracking] =
    Cogen[Coordinates].contramap(_.value)

  given Arbitrary[CompositeTracking] =
    Arbitrary:
      for {
        len <- Gen.choose(2, 5)
        ts  <- Gen.listOfN(len, arbitrary[Tracking](using arbTracking))
      } yield CompositeTracking(NonEmptyList.fromListUnsafe(ts))

  given Cogen[CompositeTracking] =
    Cogen[List[Tracking]](using cogenSeq(using cogenTracking)).contramap(_.toNonEmptyList.toList)

  given arbTracking: Arbitrary[Tracking] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[ConstantTracking],
        arbitrary[CompositeTracking],
        arbitrary[EphemerisTracking],
        arbitrary[SiderealTracking]
      )

  given cogenTracking: Cogen[Tracking] =
    Cogen[
      Either[
        ConstantTracking, 
        Either[
          CompositeTracking, 
          Either[
            EphemerisTracking, 
            SiderealTracking
          ]
        ]
      ]
    ].contramap:
        case t @ ConstantTracking(_)             => t.asLeft
        case t @ CompositeTracking(_)            => t.asLeft.asRight
        case t @ EphemerisTracking(_)            => t.asLeft.asRight.asRight
        case t @ SiderealTracking(_, _, _, _, _) => t.asRight.asRight.asRight    
}

object ArbTracking extends ArbTracking
