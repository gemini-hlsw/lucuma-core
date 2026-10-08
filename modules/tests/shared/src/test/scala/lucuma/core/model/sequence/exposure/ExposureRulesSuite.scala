// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.exposure

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SkyBackground
import lucuma.core.model.ConstraintSet
import lucuma.core.model.arb.ArbConstraintSet.given
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.arb.ArbStep.given
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2DynamicConfig.given
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.arb.ArbGhostDetector.given
import lucuma.core.model.sequence.ghost.arb.ArbGhostDynamicConfig.given
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.model.sequence.gmos.arb.ArbDynamicConfig.given
import lucuma.core.model.sequence.gmos.arb.ArbGmosGratingConfig.given
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.arb.ArbIgrins2DynamicConfig.given
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import munit.FunSuite
import org.scalacheck.Arbitrary

import scala.collection.immutable.SortedSet

import ExposureTimeViolation.Severity

class ExposureRulesSuite extends FunSuite:

  // ExposureTimeLimits

  import ExposureTimeLimits.Result

  test("limits are inclusive"):
    val l = ExposureTimeLimits.unsafeFromMinMax(1.secTimeSpan, 20.secTimeSpan)
    assertEquals(l.check(500.msTimeSpan), Result.Below(1.secTimeSpan))
    assertEquals(l.check(1.secTimeSpan), Result.InRange)
    assertEquals(l.check(20.secTimeSpan), Result.InRange)
    assertEquals(l.check(21.secTimeSpan), Result.Above(20.secTimeSpan))

  test("one-sided limits"):
    assertEquals(ExposureTimeLimits.fromMin(1.secTimeSpan).check(500.msTimeSpan), Result.Below(1.secTimeSpan))
    assertEquals(ExposureTimeLimits.fromMax(1.secTimeSpan).check(2.secTimeSpan), Result.Above(1.secTimeSpan))
    assert(ExposureTimeLimits.fromMin(1.secTimeSpan).isValid(1.secTimeSpan))
    assert(!ExposureTimeLimits.fromMax(1.secTimeSpan).isValid(2.secTimeSpan))

  test("a missing limit is not checked"):
    assertEquals(ExposureTimeLimits.fromMin(1.secTimeSpan).check(1000.secTimeSpan), Result.InRange)
    assertEquals(ExposureTimeLimits.fromMax(1.secTimeSpan).check(TimeSpan.Zero), Result.InRange)

  test("fromMinMax rejects a minimum above the maximum"):
    assertEquals(ExposureTimeLimits.fromMinMax(20.secTimeSpan, 1.secTimeSpan), none)
    assert(ExposureTimeLimits.fromMinMax(1.secTimeSpan, 1.secTimeSpan).isDefined)
    intercept[IllegalArgumentException](ExposureTimeLimits.unsafeFromMinMax(20.secTimeSpan, 1.secTimeSpan))

  test("whole seconds"):
    val r = ExposureRule.WholeSeconds(NonEmptyString.unsafeFrom("GMOS North"))
    assertEquals(r.check(3.secTimeSpan), none)
    assertEquals(r.check(1500.msTimeSpan).map(_.isError), true.some)

  // ExposureRules, called directly as an editor would

  private def secs(s: BigDecimal): TimeSpan =
    TimeSpan.unsafeFromMicroseconds((s * 1_000_000).toLongExact)

  private def check(rules: List[ExposureRule], time: TimeSpan): List[ExposureTimeViolation] =
    ExposureTimeViolation.check(rules, time, ObserveClass.Science)

  test("GNIRS read mode limits"):
    assertEquals(check(ExposureRules.gnirs(GnirsReadMode.Bright), secs(10)), Nil)
    assertEquals(check(ExposureRules.gnirs(GnirsReadMode.Bright), secs(0.5)).map(_.isError), List(true))
    assertEquals(check(ExposureRules.gnirs(GnirsReadMode.Bright), secs(30)).map(_.isError), List(false))
    assertEquals(check(ExposureRules.gnirs(GnirsReadMode.VeryFaint), secs(3600)), Nil)

  test("errors hide warnings when checking rules directly"):
    val rules = ExposureRules.flamingos2(Flamingos2ReadMode.Bright)
    assertEquals(rules.flatMap(_.check(TimeSpan.Zero)).map(_.isError), List(true, false))
    assertEquals(check(rules, TimeSpan.Zero).map(_.isError), List(true))
    assertEquals(ExposureTimeViolation.check(rules, TimeSpan.Zero, ObserveClass.NightCal).map(_.isError), List(true))

  test("checking rules directly applies the same selection as checking a step"):
    // Warnings are for science exposures alone.
    assertEquals(ExposureTimeViolation.check(ExposureRules.gnirs(GnirsReadMode.Bright), secs(30), ObserveClass.Acquisition), Nil)
    // IGRINS-2 acquisitions use the slit viewing camera.
    assertEquals(ExposureTimeViolation.check(ExposureRules.igrins2(ObserveClass.Acquisition), secs(2), ObserveClass.Acquisition), Nil)
    assertEquals(ExposureTimeViolation.check(ExposureRules.igrins2(ObserveClass.Science), secs(2), ObserveClass.Science).map(_.isError), List(true))
    // GMOS acquisitions are not checked for saturation.
    def gmosImaging(oc: ObserveClass): List[ExposureRule] =
      ExposureRules.gmosNorth(ExposureRules.GmosImaging(oc, GmosNorthFilter.RPrime.some, GmosXBinning.One, GmosYBinning.One, GmosAmpGain.Low).some, SkyBackground.Bright)
    assertEquals(ExposureTimeViolation.check(gmosImaging(ObserveClass.Acquisition), secs(500), ObserveClass.Acquisition), Nil)
    assertEquals(ExposureTimeViolation.check(gmosImaging(ObserveClass.Science), secs(500), ObserveClass.Science).map(_.isError), List(true))

  test("violations carry the severity and the rule as a sentence"):
    assertEquals(
      check(ExposureRules.gnirs(GnirsReadMode.VeryFaint), secs(5)),
      List(ExposureTimeViolation(Severity.Error, "Exposure times for GNIRS very faint read mode must be at least 18 s."))
    )
    assertEquals(
      check(ExposureRules.gmosNorth(none, SkyBackground.Dark), secs(1500)),
      List(ExposureTimeViolation(Severity.Warning, "Exposure times above 1200 s are not recommended for GMOS North due to cosmic ray contamination."))
    )

  // ExposureTimeViolation.check, applied to sequence steps

  private def sample[A](using a: Arbitrary[A]): A =
    Iterator.continually(a.arbitrary.sample).flatten.next()

  private def ctx(sb: SkyBackground): PendingExposureRules.Context =
    PendingExposureRules.Context(sample[ConstraintSet].copy(skyBackground = sb))

  private def gmosNorth(
    seconds:      BigDecimal,
    filter:       Option[GmosNorthFilter] = GmosNorthFilter.RPrime.some,
    imaging:      Boolean                 = true,
    binning:      GmosBinning             = GmosBinning.One,
    yBinning:     Option[GmosBinning]     = none,
    gain:         GmosAmpGain             = GmosAmpGain.Low,
    stepConfig:   StepConfig              = StepConfig.Science,
    observeClass: ObserveClass            = ObserveClass.Science
  ): Step[DynamicConfig.GmosNorth] =
    val d0 = sample[DynamicConfig.GmosNorth]
    val d  = d0.copy(
      exposure      = secs(seconds),
      readout       = GmosCcdMode.ampGain.replace(gain)(GmosCcdMode.xBin.replace(GmosXBinning(binning))(GmosCcdMode.yBin.replace(GmosYBinning(yBinning.getOrElse(binning)))(d0.readout))),
      filter        = filter,
      gratingConfig = Option.unless(imaging)(sample[GmosGratingConfig.North]),
      fpu           = if imaging then none else d0.fpu
    )
    sample[Step[DynamicConfig.GmosNorth]].copy(instrumentConfig = d, stepConfig = stepConfig, observeClass = observeClass)

  private def violations(sb: SkyBackground, step: Step[DynamicConfig.GmosNorth]): List[String] =
    ExposureTimeViolation.check(step, ctx(sb)).map(_.description)

  private val CosmicRays = "Exposure times above 1200 s are not recommended for GMOS North due to cosmic ray contamination."

  private def saturation(sb: String, limit: Int, recommended: Boolean, bin: Int = 1, yBin: Option[Int] = none): String =
    val subject = s"GMOS North r imaging with ${bin}x${yBin.getOrElse(bin)} binning under a $sb sky"
    if recommended then s"Exposure times above $limit s are not recommended for $subject due to sky background saturation."
    else s"Exposure times for $subject must be at most $limit s due to sky background saturation."

  test("GMOS North r imaging under a Dark sky saturates at 6396 s and warns from half that"):
    assertEquals(violations(SkyBackground.Dark, gmosNorth(1200)), Nil)
    assertEquals(violations(SkyBackground.Dark, gmosNorth(3198)), List(CosmicRays))
    assertEquals(violations(SkyBackground.Dark, gmosNorth(3199)), List(CosmicRays, saturation("Dark", 3198, true)))

  test("errors hide warnings"):
    assertEquals(violations(SkyBackground.Dark, gmosNorth(6397)), List(saturation("Dark", 6396, false)))

  test("GMOS saturation depends on the sky background"):
    assertEquals(violations(SkyBackground.Bright, gmosNorth(457)), List(saturation("Bright", 456, false)))
    assertEquals(violations(SkyBackground.Darkest, gmosNorth(457)), Nil)

  test("GMOS saturation scales with the square of the binning"):
    assertEquals(violations(SkyBackground.Bright, gmosNorth(115, binning = GmosBinning.Two)), List(saturation("Bright", 114, false, 2)))
    assertEquals(violations(SkyBackground.Bright, gmosNorth(114, binning = GmosBinning.Two)), List(saturation("Bright", 57, true, 2)))

  test("GMOS saturation scales with the number of pixels binned, square or not"):
    assertEquals(violations(SkyBackground.Bright, gmosNorth(229, binning = GmosBinning.Two, yBinning = GmosBinning.One.some)), List(saturation("Bright", 228, false, 2, 1.some)))
    assertEquals(violations(SkyBackground.Bright, gmosNorth(229, binning = GmosBinning.One, yBinning = GmosBinning.Two.some)), List(saturation("Bright", 228, false, 1, 2.some)))
    assertEquals(violations(SkyBackground.Bright, gmosNorth(228, binning = GmosBinning.One, yBinning = GmosBinning.Two.some)), List(saturation("Bright", 114, true, 1, 2.some)))

  test("GMOS saturation is checked only for low gain science imaging in a listed filter, not acquisitions"):
    assertEquals(violations(SkyBackground.Bright, gmosNorth(500, gain = GmosAmpGain.High)), Nil)
    assertEquals(violations(SkyBackground.Bright, gmosNorth(500, imaging = false)), Nil)
    assertEquals(violations(SkyBackground.Bright, gmosNorth(500, stepConfig = StepConfig.Dark)), Nil)
    assertEquals(violations(SkyBackground.Bright, gmosNorth(500, observeClass = ObserveClass.Acquisition)), Nil)
    assertEquals(violations(SkyBackground.Bright, gmosNorth(500, filter = GmosNorthFilter.HeII.some)), Nil)
    assertEquals(violations(SkyBackground.Bright, gmosNorth(500, filter = none)), Nil)

  test("warnings apply to science steps alone"):
    assertEquals(violations(SkyBackground.Darkest, gmosNorth(1201, imaging = false, observeClass = ObserveClass.NightCal)), Nil)
    assertEquals(violations(SkyBackground.Darkest, gmosNorth(1201, imaging = false)), List(CosmicRays))

  test("errors apply to every step, and all are reported"):
    assertEquals(
      violations(SkyBackground.Darkest, gmosNorth(0.5, imaging = false, observeClass = ObserveClass.NightCal)),
      List("Exposure times for GMOS North must be at least 1 s.", "Exposure times for GMOS North must be a whole number of seconds.")
    )
    assertEquals(
      violations(SkyBackground.Darkest, gmosNorth(1.5, imaging = false, observeClass = ObserveClass.NightCal)),
      List("Exposure times for GMOS North must be a whole number of seconds.")
    )

  test("IGRINS-2 acquisitions use the slit viewing camera limits"):
    def igrins2(seconds: BigDecimal, observeClass: ObserveClass): List[String] =
      val step = sample[Step[Igrins2DynamicConfig]].copy(
        instrumentConfig = Igrins2DynamicConfig(secs(seconds)),
        stepConfig       = StepConfig.Science,
        observeClass     = observeClass
      )
      ExposureTimeViolation.check(step, ctx(SkyBackground.Darkest)).map(_.description)

    assertEquals(igrins2(2, ObserveClass.Acquisition), Nil)
    assertEquals(igrins2(1, ObserveClass.Acquisition), List("Exposure times for the IGRINS-2 slit viewing camera must be at least 1.63 s."))
    assertEquals(igrins2(2, ObserveClass.Science), List("Exposure times for IGRINS-2 must be at least 3.08 s."))

  // Every instrument, checked through its PendingExposureRules instance

  private def describe[D](d: D, observeClass: ObserveClass = ObserveClass.Science, sb: SkyBackground = SkyBackground.Darkest)(
    using Arbitrary[Step[D]], PendingExposureRules[D]
  ): List[String] =
    val step = sample[Step[D]].copy(instrumentConfig = d, stepConfig = StepConfig.Science, observeClass = observeClass)
    ExposureTimeViolation.check(step, ctx(sb)).map(_.description)

  private def flamingos2(seconds: BigDecimal, readMode: Flamingos2ReadMode): Flamingos2DynamicConfig =
    sample[Flamingos2DynamicConfig].copy(exposure = secs(seconds), readMode = readMode)

  test("Flamingos 2 read mode minimums, recommended minimums and whole seconds"):
    assertEquals(describe(flamingos2(5, Flamingos2ReadMode.Bright)), Nil)
    assertEquals(describe(flamingos2(3, Flamingos2ReadMode.Bright)), List("Exposure times below 5 s are not recommended for Flamingos 2 bright read mode."))
    assertEquals(describe(flamingos2(3, Flamingos2ReadMode.Bright), ObserveClass.NightCal), Nil)
    assertEquals(describe(flamingos2(1, Flamingos2ReadMode.Bright)), List("Exposure times for Flamingos 2 bright read mode must be at least 1.5 s."))
    assertEquals(describe(flamingos2(80, Flamingos2ReadMode.Faint)), List("Exposure times below 85 s are not recommended for Flamingos 2 faint read mode."))
    assertEquals(describe(flamingos2(21.5, Flamingos2ReadMode.Medium)), List("Exposure times for Flamingos 2 must be a whole number of seconds."))

  private def ghost(red: BigDecimal, blue: BigDecimal): GhostDynamicConfig =
    GhostDynamicConfig(
      GhostDetector.Red(sample[GhostDetector].copy(exposureTime = secs(red))),
      GhostDetector.Blue(sample[GhostDetector].copy(exposureTime = secs(blue)))
    )

  test("GHOST checks each camera's exposure separately"):
    assertEquals(describe(ghost(100, 1800)), Nil)
    assertEquals(describe(ghost(0.05, 100)), List("Exposure times for the GHOST red camera must be at least 0.1 s."))
    assertEquals(describe(ghost(100, 2000)), List("Exposure times above 1800 s are not recommended for the GHOST blue camera due to cosmic ray contamination."))
    assertEquals(describe(ghost(4000, 100)), List("Exposure times for the GHOST red camera must be at most 3600 s."))
    assertEquals(
      describe(ghost(0.05, 2000)),
      List("Exposure times for the GHOST red camera must be at least 0.1 s.", "Exposure times above 1800 s are not recommended for the GHOST blue camera due to cosmic ray contamination.")
    )

  private def gmosSouth(seconds: BigDecimal, imaging: Boolean = true): DynamicConfig.GmosSouth =
    val d0 = sample[DynamicConfig.GmosSouth]
    d0.copy(
      exposure      = secs(seconds),
      readout       = GmosCcdMode.ampGain.replace(GmosAmpGain.Low)(GmosCcdMode.xBin.replace(GmosXBinning.One)(GmosCcdMode.yBin.replace(GmosYBinning.One)(d0.readout))),
      filter        = GmosSouthFilter.RPrime.some,
      gratingConfig = Option.unless(imaging)(sample[GmosGratingConfig.South]),
      fpu           = if imaging then none else d0.fpu
    )

  test("GMOS South uses its own saturation times"):
    val saturates = "Exposure times for GMOS South r imaging with 1x1 binning under a Bright sky must be at most 258 s due to sky background saturation."
    assertEquals(describe(gmosSouth(259), sb = SkyBackground.Bright), List(saturates))
    assertEquals(describe(gmosSouth(130), sb = SkyBackground.Bright), List("Exposure times above 129 s are not recommended for GMOS South r imaging with 1x1 binning under a Bright sky due to sky background saturation."))
    assertEquals(describe(gmosSouth(129), sb = SkyBackground.Bright), Nil)
    assertEquals(
      describe(gmosSouth(0.5, imaging = false), ObserveClass.NightCal),
      List("Exposure times for GMOS South must be at least 1 s.", "Exposure times for GMOS South must be a whole number of seconds.")
    )
    assertEquals(describe(gmosSouth(1201, imaging = false)), List("Exposure times above 1200 s are not recommended for GMOS South due to cosmic ray contamination."))

  test("IGRINS-2 maximum, for the spectrograph and the slit viewing camera"):
    assertEquals(describe(Igrins2DynamicConfig(secs(600))), Nil)
    assertEquals(describe(Igrins2DynamicConfig(secs(601))), List("Exposure times for IGRINS-2 must be at most 600 s."))
    assertEquals(describe(Igrins2DynamicConfig(secs(601)), ObserveClass.Acquisition), List("Exposure times for the IGRINS-2 slit viewing camera must be at most 600 s."))

  test("violations are ordered errors first, then by description"):
    val warning = ExposureTimeViolation(Severity.Warning, "A")
    val error1  = ExposureTimeViolation(Severity.Error,   "C")
    val error2  = ExposureTimeViolation(Severity.Error,   "B")
    assertEquals(SortedSet(warning, error1, error2).toList, List(error2, error1, warning))

  test("Eq instances"):
    val rules = ExposureRules.gmosNorth(none, SkyBackground.Dark)
    assert(rules === ExposureRules.gmosNorth(none, SkyBackground.Dark))
    assert(rules =!= ExposureRules.gmosSouth(none, SkyBackground.Dark))
    assert(ExposureTimeLimits.fromMin(1.secTimeSpan) === ExposureTimeLimits.fromMin(1.secTimeSpan))
    assert(ExposureTimeLimits.fromMin(1.secTimeSpan) =!= ExposureTimeLimits.fromMax(1.secTimeSpan))
    val v = ExposureTimeViolation.check(rules, secs(0.5), ObserveClass.Science)
    assert(v === ExposureTimeViolation.check(rules, secs(0.5), ObserveClass.Science))
    assert(v === ExposureTimeViolation.check(rules, secs(0.25), ObserveClass.Science))
    assert(v =!= ExposureTimeViolation.check(rules, secs(1.5), ObserveClass.Science))

  test("bias steps are not checked"):
    assertEquals(violations(SkyBackground.Darkest, gmosNorth(0, imaging = false, stepConfig = StepConfig.Bias)), Nil)
