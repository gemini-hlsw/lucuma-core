// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.syntax.all.*
import coulomb.*
import coulomb.rational.Rational
import eu.timepit.refined.*
import eu.timepit.refined.api.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.math.units.*
import lucuma.core.refined.given
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.NewRefined

type CloudExtinctionPredicate = Interval.Closed[0, 5000]
type CloudExtinction = CloudExtinction.Type
object CloudExtinction extends NewRefined[Extinction, CloudExtinctionPredicate]:
  inline def fromMilliVegaMagnitude(value: Short): Either[String, CloudExtinction] =
    NonNegShort.from(value).flatMap(nns => from(Extinction.fromRefined(nns)))

  inline def unsafeFromMilliVegaMagnitude(value: Short): CloudExtinction           =
    unsafeFrom(Extinction.fromRefined(NonNegShort.unsafeFrom(value)))

  inline def fromVegaMagnitude(value: BigDecimal): Either[String, CloudExtinction] =
    fromMilliVegaMagnitude((value * 1000).toShort)

  inline def unsafeFromVegaMagnitude(value: BigDecimal): CloudExtinction =
    unsafeFromMilliVegaMagnitude((value * 1000).toShort)

  extension (ce: CloudExtinction)
    def toExtinction: Extinction = ce.value.value

    def toVegaMagnitude: BigDecimal = 
      val r = toExtinction.toVegaMagnitude.value
      BigDecimal(r.n) / BigDecimal(r.d)

    def label: String = f"< ${toVegaMagnitude.toDouble}%.2f mag"

    def percentile: IntCentiPercent =
      Preset.values.find(preset => ce <= preset.toCloudExtinction).map(_.percentile).getOrElse(IntCentiPercent.Max)

  given Display[CloudExtinction] = Display.byShortName(_.label)

  enum Preset(val tag: String, val toCloudExtinction: CloudExtinction, val percentile: IntCentiPercent) derives Enumerated:
    case PointOne       extends Preset("point_one", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(100)), IntCentiPercent.unsafeFromPercent(50))
    case PointThree     extends Preset("point_three", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(300)), IntCentiPercent.unsafeFromPercent(70))
    case PointFive      extends Preset("point_five", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(500)), IntCentiPercent.unsafeFromPercent(75))
    case OnePointZero   extends Preset("one_point_zero", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(1000)), IntCentiPercent.unsafeFromPercent(80))
    case OnePointFive   extends Preset("one_point_five", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(1500)), IntCentiPercent.unsafeFromPercent(90))
    case TwoPointZero   extends Preset("two_point_zero", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(2000)), IntCentiPercent.unsafeFromPercent(95))
    case ThreePointZero extends Preset("three_point_zero", CloudExtinction.unsafeFrom(Extinction.unsafeFrom(3000)), IntCentiPercent.unsafeFromPercent(100))

  object Preset:
    given Display[Preset] = Display[CloudExtinction].contramap(_.toCloudExtinction)
