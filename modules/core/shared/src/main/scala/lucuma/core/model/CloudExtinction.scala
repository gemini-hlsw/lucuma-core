// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.syntax.contravariant.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.rational.Rational
import coulomb.syntax.*
import coulomb.units.si.prefixes.*
import eu.timepit.refined.*
import eu.timepit.refined.api.*
import eu.timepit.refined.numeric.Interval
import lucuma.core.math.units.*
import lucuma.core.math.units.given
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType

type CloudExtinctionPredicate = Interval.Closed[0, 500]
type CloudExtinctionValue  = Int Refined CloudExtinctionPredicate
val CloudExtinctionValue = new RefinedTypeOps[CloudExtinctionValue, Int]

type CloudExtinction = CloudExtinction.Type
object CloudExtinction extends NewType[Quantity[CloudExtinctionValue, CentiVegaMagnitude]]:
  def fromCentiVegaMagnitude(value: Int): Either[String, CloudExtinction] =
    CloudExtinctionValue.from(value).map(_.withUnit[CentiVegaMagnitude]).map(CloudExtinction(_))

  def unsafeFromCentiVegaMagnitude(value: Int): CloudExtinction           =
    CloudExtinction(CloudExtinctionValue.unsafeFrom(value).withUnit[CentiVegaMagnitude])

  extension (ce: CloudExtinction)
    def toCentiVegaMagnitude: Int = ce.value.value.value

    def toVegaMagnitude: Quantity[Rational, VegaMagnitude] = ce.value.toValue[Int].toValue[Rational].toUnit[VegaMagnitude]

    def label: String        = f"< ${toVegaMagnitude.value.toDouble}%.2f mag"

  given Display[CloudExtinction] = Display.byShortName(_.label)

  enum Point(val tag: String, val toCloudExtinction: CloudExtinction) derives Enumerated:
    case PointOne       extends Point("point_one", CloudExtinction.unsafeFromCentiVegaMagnitude(10))
    case PointThree     extends Point("point_three", CloudExtinction.unsafeFromCentiVegaMagnitude(30))
    case PointFive      extends Point("point_five", CloudExtinction.unsafeFromCentiVegaMagnitude(50))
    case OnePointZero   extends Point("one_point_zero", CloudExtinction.unsafeFromCentiVegaMagnitude(100))
    case OnePointFive   extends Point("one_point_five", CloudExtinction.unsafeFromCentiVegaMagnitude(150))
    case TwoPointZero   extends Point("two_point_zero", CloudExtinction.unsafeFromCentiVegaMagnitude(200))
    case ThreePointZero extends Point("three_point_zero", CloudExtinction.unsafeFromCentiVegaMagnitude(300))

  object Point:
    given Display[Point] = Display[CloudExtinction].contramap(_.toCloudExtinction)
