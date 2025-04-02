// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.syntax.contravariant.*
import coulomb.*
import coulomb.rational.Rational
import eu.timepit.refined.*
import eu.timepit.refined.api.*
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.math.units.*
import lucuma.core.refined.given
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType

type CloudExtinctionPredicate = Interval.Closed[0, 5000]

private type CloudExtinctionType  = Extinction Refined CloudExtinctionPredicate
private val CloudExtinctionType = new RefinedTypeOps[CloudExtinctionType, Extinction]

type CloudExtinction = CloudExtinction.Type
object CloudExtinction extends NewType[CloudExtinctionType]:
  def from(value: Extinction): Either[String, CloudExtinction] =
    CloudExtinctionType.from(value).map(CloudExtinction(_))

  def unsafeFrom(value: Extinction): CloudExtinction =
    CloudExtinction(CloudExtinctionType.unsafeFrom(value))

  def fromMilliVegaMagnitude(value: NonNegShort): Either[String, CloudExtinction] =
    from(Extinction(value))

  def unsafeFromMilliVegaMagnitude(value: NonNegShort): CloudExtinction           =
    unsafeFrom(Extinction(value))

  extension (ce: CloudExtinction)
    def toExtinction: Extinction = ce.value.value

    def toVegaMagnitude: Quantity[Rational, VegaMagnitude] = 
      toExtinction.toVegaMagnitude

    def label: String        = f"< ${toVegaMagnitude.value.toDouble}%.2f mag"

  given Display[CloudExtinction] = Display.byShortName(_.label)

  enum Point(val tag: String, val toCloudExtinction: CloudExtinction) derives Enumerated:
    case PointOne       extends Point("point_one", CloudExtinction.unsafeFromMilliVegaMagnitude(NonNegShort.unsafeFrom(100)))
    case PointThree     extends Point("point_three", CloudExtinction.unsafeFromMilliVegaMagnitude(NonNegShort.unsafeFrom(300)))
    case PointFive      extends Point("point_five", CloudExtinction.unsafeFromMilliVegaMagnitude(NonNegShort.unsafeFrom(500)))
    case OnePointZero   extends Point("one_point_zero", CloudExtinction.unsafeFromMilliVegaMagnitude(NonNegShort.unsafeFrom(1000)))
    case OnePointFive   extends Point("one_point_five", CloudExtinction.unsafeFromMilliVegaMagnitude(NonNegShort.unsafeFrom(1500)))
    case TwoPointZero   extends Point("two_point_zero", CloudExtinction.unsafeFromMilliVegaMagnitude(NonNegShort.unsafeFrom(2000)))
    case ThreePointZero extends Point("three_point_zero", CloudExtinction.unsafeFromMilliVegaMagnitude(NonNegShort.unsafeFrom(3000)))

  object Point:
    given Display[Point] = Display[CloudExtinction].contramap(_.toCloudExtinction)
