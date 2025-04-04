// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.syntax.contravariant.*
import cats.syntax.all.*
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
import lucuma.core.util.NewRefined
import eu.timepit.refined.cats.given

type CloudExtinctionPredicate = Interval.Closed[0, 5000]
type CloudExtinction = CloudExtinction.Type
object CloudExtinction extends NewRefined[Extinction, CloudExtinctionPredicate]:
  def fromMilliVegaMagnitude(value: NonNegShort): Either[String, CloudExtinction] =
    from(Extinction.fromRefined(value))

  def unsafeFromMilliVegaMagnitude(value: NonNegShort): CloudExtinction           =
    unsafeFrom(Extinction.fromRefined(value))

  extension (ce: CloudExtinction)
    def toExtinction: Extinction = ce.value.value

    def toVegaMagnitude: Quantity[Rational, VegaMagnitude] = 
      toExtinction.toVegaMagnitude

    def label: String        = f"< ${toVegaMagnitude.value.toDouble}%.2f mag"

    def percentile: Percentile =
      Preset.values.find(preset => ce <= preset.toCloudExtinction).map(_.percentile).getOrElse(Percentile.Max)

  given Display[CloudExtinction] = Display.byShortName(_.label)

  enum Preset(val tag: String, val toCloudExtinction: CloudExtinction, val percentile: Percentile) derives Enumerated:
    case PointOne       extends Preset("point_one", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(100)), Percentile.unsafeFromPercent(50))
    case PointThree     extends Preset("point_three", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(300)), Percentile.unsafeFromPercent(70))
    case PointFive      extends Preset("point_five", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(500)), Percentile.unsafeFromPercent(75))
    case OnePointZero   extends Preset("one_point_zero", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(1000)), Percentile.unsafeFromPercent(80))
    case OnePointFive   extends Preset("one_point_five", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(1500)), Percentile.unsafeFromPercent(90))
    case TwoPointZero   extends Preset("two_point_zero", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(2000)), Percentile.unsafeFromPercent(95))
    case ThreePointZero extends Preset("three_point_zero", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(3000)), Percentile.unsafeFromPercent(100))

  object Preset:
    given Display[Preset] = Display[CloudExtinction].contramap(_.toCloudExtinction)
