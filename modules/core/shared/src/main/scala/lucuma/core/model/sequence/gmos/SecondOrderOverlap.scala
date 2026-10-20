// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.syntax.all.*
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepViolation

/**
 * Whether second order light may overlap the spectrum of a GMOS spectroscopy
 * science step.  A grating sends second order light of wavelength λ to where
 * first order light of 2λ falls, so if the shortest wavelength reaching the
 * grating is λmin, everything past 2λmin is exposed to second order light.  λmin
 * is the filter's cut-on or, without a filter, `NoFilterBlueLimit`.  A step is
 * affected when its wavelength coverage reaches past 2λmin and the filter passes
 * first order light there.  (A band pass filter that ends before 2λmin leaves
 * only second order light past it.)
 *
 * The rule is in lucuma-core so that editors can check steps as they are
 * edited, the same way the ODB does.
 */
object SecondOrderOverlap:

  /**
   * The shortest wavelength reaching the grating when there is no filter.  With
   * no filter there is no sharp cut-on, so this is the approximate blue limit of
   * GMOS sensitivity: the point below which the atmosphere (ozone absorption),
   * the telescope and GMOS optics, and the detector quantum efficiency together
   * pass little light.  The science team chose 360 nm as a starting value and
   * may adjust it in the future (sc-10811), which takes a lucuma-core release.
   */
  val NoFilterBlueLimit: Wavelength =
    Wavelength.unsafeFromIntPicometers(360_000)

  /**
   * Where second order light starts, given the filter's transmission range if
   * any.  `None` when it falls beyond the longest representable wavelength.
   */
  def onset(filterRange: Option[BoundedInterval[Wavelength]]): Option[Wavelength] =
    val pm = filterRange.fold(NoFilterBlueLimit)(_.lower).pm.value.value.toLong * 2
    Option.when(pm.isValidInt)(pm.toInt).flatMap(Wavelength.fromIntPicometers)

  /**
   * The message for an overlap starting at `onset`.  It doesn't name the step,
   * so the steps sharing an onset (e.g., wavelength dithers) report it once.
   */
  def message(onset: Wavelength): String =
    s"Second order light overlaps the spectrum beyond ${formatNm(onset)}. " +
      "It is recommended to use a blocking filter to avoid second order overlap."

  private def formatNm(w: Wavelength): String =
    s"${Wavelength.decimalNanometers.reverseGet(w).bigDecimal.stripTrailingZeros.toPlainString} nm"

  /**
   * The onset of second order light if it falls within the first order
   * spectrum of the given coverage centered at `centralWavelength`, behind a
   * filter with the given transmission range.
   */
  def overlap(
    coverage:          WavelengthDelta,
    centralWavelength: Wavelength,
    filterRange:       Option[BoundedInterval[Wavelength]]
  ): Option[Wavelength] =
    onset(filterRange)
      .filter(o => filterRange.forall(o < _.upper) && o < coverage.centeredAt(centralWavelength).upper)

  private def violation(onset: Wavelength): StepViolation =
    StepViolation(ObservationValidationCode.SecondOrderOverlap, message(onset))

  private def science[D](step: Step[D])(f: D => Option[Wavelength]): Option[StepViolation] =
    step.stepConfig match
      case StepConfig.Science => f(step.instrumentConfig).map(violation)
      case _                  => none

  /** Checks a GMOS North step; only science steps with a grating can overlap. */
  def gmosNorth(step: Step[DynamicConfig.GmosNorth]): Option[StepViolation] =
    science(step): d =>
      d.gratingConfig.flatMap(g => overlap(g.grating.simultaneousCoverage, g.wavelength, d.filter.map(_.width)))

  /** Checks a GMOS South step; only science steps with a grating can overlap. */
  def gmosSouth(step: Step[DynamicConfig.GmosSouth]): Option[StepViolation] =
    science(step): d =>
      d.gratingConfig.flatMap(g => overlap(g.grating.simultaneousCoverage, g.wavelength, d.filter.map(_.width)))
