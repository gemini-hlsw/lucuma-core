// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Order
import cats.Order.given
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.syntax.all.*
import fs2.*
import lucuma.ags.AgsAnalysis.*
import lucuma.ags.AgsGuideQuality.*
import lucuma.catalog.BrightnessConstraints
import lucuma.core.enums.Band
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.StepGuideState
import lucuma.core.geom.Shape
import lucuma.core.geom.offsets.OffsetPosition
import lucuma.core.geom.offsets.OffsetPositions
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ImageQuality
import lucuma.core.model.sequence.TelescopeConfig

import scala.collection.immutable.SortedSet

object Ags {
  private case class AgsContextBuffer(
    guideSpeeds:          List[(GuideSpeed, BrightnessConstraints)],
    // Positions paired with their geometry, in generation order, so the loop never looks up.
    positionCalcs:        List[(OffsetPosition, AgsGeomCalc)],
    brightnessConstraint: Option[BrightnessConstraints],
    calcsNanos:           Long // time spent in posCalculations
  ):
    def guideSpeedOf(candidate: GuideStarCandidate): Option[GuideSpeed] =
      guideSpeedFor(guideSpeeds, candidate)

  private def satisfies(constraint: BrightnessConstraints, candidate: GuideStarCandidate): Boolean =
    candidate
      .brightnessIn(constraint.searchBands)
      .exists((band, brightness) => constraint.contains(band, brightness))

  private def guideSpeedFor(
    speeds:    List[(GuideSpeed, BrightnessConstraints)],
    candidate: GuideStarCandidate
  ): Option[GuideSpeed] =
    speeds.find((_, constraint) => satisfies(constraint, candidate)).map(_._1)

  private def withinConstraint(
    constraint: Option[BrightnessConstraints],
    candidate:  GuideStarCandidate
  ): Boolean =
    constraint.exists(satisfies(_, candidate))

  def resultLabel(a: AgsAnalysis): String = a match
    case _: Usable                 => "usable"
    case _: NotReachableAtPosition => "not_reachable"
    case _: VignettesScience       => "vignettes_science"
    case _: NoGuideStarForProbe    => "no_guide_star"
    case _: NoMagnitudeForBand     => "no_magnitude"
    case _: MagnitudeTooFaint      => "magnitude_too_faint"
    case _: MagnitudeTooBright     => "magnitude_too_bright"

  // Runs the analysis for a single guide star at a single position. `guideSpeed` is the
  // candidate's fastest usable speed, computed once per candidate by the caller.
  protected def runAnalysis(
    conditions:     ConstraintSet,
    gsOffset:       Offset,
    protectedAreas: List[Shape],
    pos:            OffsetPosition,
    params:         AgsParams,
    candidate:      GuideStarCandidate,
    guideSpeed:     Option[GuideSpeed],
    geoms:          AgsGeomCalc
  ): AgsAnalysis =
    if (!geoms.isReachable(gsOffset))
      AgsAnalysis.NotReachableAtPosition(pos, params.probe, guideSpeed, candidate)
    else if (protectedAreas.exists(ps => geoms.overlapsProtectedArea(gsOffset, ps)))
      AgsAnalysis.VignettesScience(candidate, pos)
    else
      magnitudeAnalysis(conditions, params.probe, gsOffset, candidate, geoms, pos, guideSpeed)

  /**
   * Analysis of the suitability of the magnitude of the given guide star regardless of its
   * reachability.
   */
  protected def magnitudeAnalysis(
    constraints: ConstraintSet,
    guideProbe:  GuideProbe,
    gsOffset:    Offset,
    guideStar:   GuideStarCandidate,
    geoms:       AgsGeomCalc,
    position:    OffsetPosition,
    guideSpeed:  Option[GuideSpeed]
  ): AgsAnalysis = {

    // Called when we know that a valid guide speed can be chosen for the given guide star.
    // Determine the quality and return an analysis indicating that the star is usable.
    def usable(guideSpeed: GuideSpeed): AgsAnalysis = {
      def worseOrEqual(iq: ImageQuality.Preset) = constraints.imageQuality >= iq

      val quality = guideSpeed match {
        case GuideSpeed.Fast   =>
          DeliversRequestedIq
        case GuideSpeed.Medium =>
          // TODO Review this limit
          if (worseOrEqual(ImageQuality.Preset.PointSix)) DeliversRequestedIq
          else PossibleIqDegradation
        case GuideSpeed.Slow   =>
          // TODO Review this limit
          if (worseOrEqual(ImageQuality.Preset.PointEight)) DeliversRequestedIq
          // TODO Review this limit
          else if (worseOrEqual(ImageQuality.Preset.PointSix)) PossibleIqDegradation
          else IqDegradation
      }

      Usable(guideProbe,
             guideStar,
             guideSpeed,
             quality,
             position.posAngle,
             geoms.vignettingArea(gsOffset)
      )
    }

    // Do we have a magnitude in the probe's band
    guideStar.brightnessIn(probeBands(guideProbe)) match {
      case Some(_) =>
        guideSpeed
          .map(usable)
          .getOrElse(NoGuideStarForProbe(guideProbe, guideStar, position.posAngle))
      case _       => NoMagnitudeForBand(guideProbe, guideStar, position.posAngle)
    }
  }

  def generatePositions(
    baseCoordinates: Option[Coordinates],
    blindOffset:     Option[Coordinates],
    posAngles:       NonEmptyList[Angle],
    acqOffsets:      Option[AcquisitionOffsets],
    scienceOffsets:  Option[ScienceOffsets]
  ): OffsetPositions =
    given Order[Angle] = Angle.SignedAngleOrder

    OffsetPositions
      .fromTelescopeConfigs(
        baseCoordinates,
        blindOffset,
        NonEmptySet.fromSetUnsafe(SortedSet.from(posAngles.toList)),
        acqOffsets.map(os => os.value.map(o => TelescopeConfig(o.value, StepGuideState.Enabled))),
        scienceOffsets.map(os =>
          os.value.map(o => TelescopeConfig(o.value, StepGuideState.Enabled))
        )
      )

  private def analysisContext(
    constraints: ConstraintSet,
    wavelength:  Wavelength,
    positions:   NonEmptyList[OffsetPosition],
    params:      AgsParams
  ): AgsContextBuffer = {
    val guideSpeeds   = guideSpeedLimits(constraints, params, wavelength)
    val calcsStart    = System.nanoTime()
    val calcs         = params.posCalculations(positions)
    val calcsNanos    = System.nanoTime() - calcsStart
    val bc            = constraintsFor(guideSpeeds)
    val byPosition    = calcs.toSortedMap
    val positionCalcs = positions.toList.map(p => (p, byPosition(p)))
    AgsContextBuffer(guideSpeeds, positionCalcs, bc, calcsNanos)
  }

  /**
   * FS2 pipe to do analysis of a stream of Candidate Guide Stars. The base and candidates must be
   * PM corrected by the caller.
   */
  def agsAnalysisStream[F[_]](
    constraints:        ConstraintSet,
    wavelength:         Wavelength,
    baseCoordinates:    Coordinates,
    scienceCoordinates: List[Coordinates],
    blindOffset:        Option[Coordinates],
    posAngles:          NonEmptyList[Angle],
    acquisitionOffsets: Option[AcquisitionOffsets],
    scienceOffsets:     Option[ScienceOffsets],
    params:             AgsParams
  ): Pipe[F, GuideStarCandidate, AgsAnalysis] = {
    val positions =
      generatePositions(
        baseCoordinates.some,
        blindOffset,
        posAngles,
        acquisitionOffsets,
        scienceOffsets
      ).value.toNonEmptyList
    val ctx       = analysisContext(constraints, wavelength, positions, params)

    val sciOffsets      = scienceCoordinates.map(baseCoordinates.diff(_).offset)
    val noZones         = blindOffset
      .map(baseCoordinates.diff(_).offset)
      .fold(sciOffsets)(_ :: sciOffsets)
    val protectedShapes = params.protectedAreas(noZones)

    in =>
      (in.filter(withinConstraint(ctx.brightnessConstraint, _)),
       Stream.emits[F, (OffsetPosition, AgsGeomCalc)](ctx.positionCalcs)
      )
        .mapN { case (candidate, (position, geoms)) =>
          val offset = baseCoordinates.diff(candidate.tracking.baseCoordinates).offset
          runAnalysis(
            constraints,
            offset,
            protectedShapes,
            position,
            params,
            candidate,
            ctx.guideSpeedOf(candidate),
            geoms
          )
        }
  }

  // Create a BrightnessConstrait that woulld include enough to calculate
  // Fast and Slow speedds
  private def constraintsFor(
    limits: List[(GuideSpeed, BrightnessConstraints)]
  ): Option[BrightnessConstraints] =
    // use the slowest speed to filter out
    (limits.find(_._1 === GuideSpeed.Slow).map(_._2),
     limits.find(_._1 === GuideSpeed.Fast).map(_._2)
    ).mapN(_ ∪ _)

  /**
   * Do analysis of a list of Candidate Guide Stars. Note the base coordinates should be pm
   * corrected if needed.
   *
   * The results contains every possible combination of candidates and positions, the result will be
   * less than candidates.length * positions.length as some candidates will be filtered out Added
   * statistics to analyze the cost of an analysis run.
   */
  def agsAnalysis(
    constraints:        ConstraintSet,
    wavelength:         Wavelength,
    baseCoordinates:    Coordinates,
    scienceCoordinates: List[Coordinates],
    blindOffset:        Option[Coordinates],
    posAngles:          NonEmptyList[Angle],
    acquisitionOffsets: Option[AcquisitionOffsets],
    scienceOffsets:     Option[ScienceOffsets],
    params:             AgsParams,
    candidates:         List[GuideStarCandidate]
  ): AgsAnalysisResult = {
    val positions =
      generatePositions(
        baseCoordinates.some,
        blindOffset,
        posAngles,
        acquisitionOffsets,
        scienceOffsets
      ).value.toNonEmptyList

    val ctxStart = System.nanoTime()
    val ctx      = analysisContext(constraints, wavelength, positions, params)
    val ctxEnd   = System.nanoTime()

    val accepted = candidates.filter(withinConstraint(ctx.brightnessConstraint, _))

    val sciOffsets      = scienceCoordinates.map(baseCoordinates.diff(_).offset)
    val noZones         = blindOffset
      .map(baseCoordinates.diff(_).offset)
      .fold(sciOffsets)(_ :: sciOffsets)
    val protectedShapes = params.protectedAreas(noZones)

    val anStart  = System.nanoTime()
    val analyses = accepted.flatMap: candidate =>
      val offset     = baseCoordinates.diff(candidate.tracking.baseCoordinates).offset
      val guideSpeed = ctx.guideSpeedOf(candidate)
      ctx.positionCalcs.map: (position, geoms) =>
        runAnalysis(
          constraints,
          offset,
          protectedShapes,
          position,
          params,
          candidate,
          guideSpeed,
          geoms
        )
    val anEnd    = System.nanoTime()

    AgsAnalysisResult.from(
      candidates.size,
      accepted.size,
      posAngles.size,
      acquisitionOffsets.fold(0)(_.value.size.toInt),
      scienceOffsets.fold(0)(_.value.size.toInt),
      positions.size,
      analyses,
      ctxEnd - ctxStart,
      ctx.calcsNanos,
      anEnd - anStart
    )
  }

  /**
   * Determines the fastest possible guide speed (if any) that may be used for guiding given a star
   * with the indicated magnitude.
   */
  def fastestGuideSpeed(
    constraints: ConstraintSet,
    probe:       GuideProbe,
    wavelength:  Wavelength,
    magnitude:   BrightnessValue
  ): Option[GuideSpeed] =
    GuideSpeed.inSpeedOrder.find: speed => // assumes the values are sorted fast to slow
      gaiaBrightnessConstraints(constraints, probe, speed, wavelength)
        .contains(Band.Gaia, magnitude)

  /**
   * Calculates brightness limits for each guide speed
   */
  def guideSpeedLimits(
    constraints: ConstraintSet,
    probe:       GuideProbe,
    wavelength:  Wavelength
  ): List[(GuideSpeed, BrightnessConstraints)] =
    GuideSpeed.inSpeedOrder.map: speed =>
      (speed, gaiaBrightnessConstraints(constraints, probe, speed, wavelength))

  /**
   * Calculates brightness limits for each guide speed, in the band the params' probe uses
   */
  def guideSpeedLimits(
    constraints: ConstraintSet,
    params:      AgsParams,
    wavelength:  Wavelength
  ): List[(GuideSpeed, BrightnessConstraints)] =
    GuideSpeed.inSpeedOrder.map: speed =>
      (speed,
       guideStarBrightnessConstraints(constraints, params.probe, params.altair, speed, wavelength)
      )

}
