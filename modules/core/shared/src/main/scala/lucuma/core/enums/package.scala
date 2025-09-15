// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.units.*
import lucuma.core.math.units.conversions.given
import spire.math.Rational

trait GmosGrating:
  def blazeWavelength:      Wavelength
  def referenceResolution:  PosInt

  /**
   * Δλ for 0.5" slit.
   * @see http://hyperphysics.phy-astr.gsu.edu/hbase/phyopt/gratres.html
   */
  private def Δλ: Double =
    blazeWavelength.nm.value.value.doubleValue / referenceResolution.value.toDouble

  /** Resolution at λ with the specified slit width. */
  def resolution(λ: Wavelength, slitWidth: Angle): Int =
    ((λ.nm.value.value.doubleValue / Δλ) * (0.5 / Angle.signedDecimalArcseconds.get(slitWidth).toDouble)).toInt

  /** Resolution at λ with the effective slit width of the given FPU. */
  def resolution(λ: Wavelength, fpu: GmosSouthFpu): Int =
    resolution(λ, fpu.effectiveSlitWidth)

private [enums] def pmToDispersion(pm: Int): Quantity[Rational, NanometersPerPixel] =
  PosInt.unsafeFrom(pm).withUnit[PicometersPerPixel].toValue[Rational].toUnit[NanometersPerPixel]

private [enums] def nmToWavelengthDelta(value: Int): WavelengthDelta =
  WavelengthDelta.fromIntNanometers(value).get

private [enums] def blazeNm(value: Int): Wavelength =
  Wavelength.fromIntNanometers(value).get

private [enums] def resolution(value: Int): PosInt =
  PosInt.unsafeFrom(value)

