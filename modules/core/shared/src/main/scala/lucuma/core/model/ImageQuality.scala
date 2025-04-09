// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.syntax.contravariant.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.accepted.*
import coulomb.units.si.prefixes.*
import eu.timepit.refined.*
import eu.timepit.refined.api.*
import eu.timepit.refined.numeric.Interval
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.erf
import lucuma.core.math.units.*
import lucuma.core.math.units.given
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.NewRefinedQuantity

import scala.math.pow

type ImageQualityPredicate = Interval.OpenClosed[0, 500]
type ImageQuality = ImageQuality.Type
object ImageQuality extends NewRefinedQuantity[Short, ImageQualityPredicate, CentiArcSecond]:
  inline def fromCentiArcSeconds(value: Short): Either[String, ImageQuality] = from(value)
  inline def unsafeFromCentiArcSeconds(value: Short): ImageQuality           = unsafeFrom(value)
  inline def fromArcSeconds(value: BigDecimal): Either[String, ImageQuality] = fromCentiArcSeconds((value * 100).toShort)
  inline def unsafeFromArcSeconds(value: BigDecimal): ImageQuality           = unsafeFromCentiArcSeconds((value * 100).toShort)

  extension (iq: ImageQuality)
    def toCentiArcSeconds: Short = iq.value.value.value

    def toArcSeconds: BigDecimal = iq.value.toValue[Short].toValue[BigDecimal].toUnit[ArcSecond].value

    def toAngle: Angle = Angle.fromMicroarcseconds(toCentiArcSeconds * 10_000L)

    def label: String = f"""< ${toArcSeconds.toDouble}%.2f"""

    /**
      * Calculate the percentile of on-source image quality for this ImageQuality as fwhm.
      */
    def percentile(wavelength: Wavelength, airmass: AirMass): IntCentiPercent =
      val zenithFwhm = iq.toArcSeconds.toDouble / pow(airmass.value.value.toDouble, 0.6)

      // model fit to QAP from 2004-2024:  (the extra +0.5 is to force 100% in the worst IQ)
      val c = Array(50.10221383, 0.87712202, 0.78467697, 16.10928544, 0.13778389, -15.8255612, 49.37405633 + 0.5)
      // The equation should give a number between 0 and 100 but rounding can give numbers below 0 or above 100. It is clamped to that range.
      val result = c(0) * erf(c(1) * pow(wavelength.toMicrometers.value.value.toDouble, c(2)) + c(3) * pow(zenithFwhm, c(4)) + c(5)) + c(6)
      IntCentiPercent.FromBigDecimal.getOption(result).getOrElse:
        if (result < 0) IntCentiPercent.Min else IntCentiPercent.Max

  given Display[ImageQuality] = Display.byShortName(_.label)

  enum Preset(val tag: String, val toImageQuality: ImageQuality) derives Enumerated:
    case PointOne     extends Preset("point_one",      ImageQuality.unsafeFromArcSeconds(0.1))
    case PointTwo     extends Preset("point_two",      ImageQuality.unsafeFromArcSeconds(0.2))
    case PointThree   extends Preset("point_three",    ImageQuality.unsafeFromArcSeconds(0.3))
    case PointFour    extends Preset("point_four",     ImageQuality.unsafeFromArcSeconds(0.4))
    case PointSix     extends Preset("point_six",      ImageQuality.unsafeFromArcSeconds(0.6))
    case PointEight   extends Preset("point_eight",    ImageQuality.unsafeFromArcSeconds(0.8))
    case OnePointZero extends Preset("one_point_zero", ImageQuality.unsafeFromArcSeconds(1.0))
    case OnePointFive extends Preset("one_point_five", ImageQuality.unsafeFromArcSeconds(1.5))
    case TwoPointZero extends Preset("two_point_zero", ImageQuality.unsafeFromArcSeconds(2.0))

  object Preset:
    given Display[Preset] = Display[ImageQuality].contramap(_.toImageQuality)
