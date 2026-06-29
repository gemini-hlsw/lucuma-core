// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Order
import cats.Order.given
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
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
import lucuma.core.geom.Area
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
    calcs:                NonEmptyMap[OffsetPosition, AgsGeomCalc],
    brightnessConstraint: Option[BrightnessConstraints],
    calcsNanos:           Long // time spent in posCalculations
  )

  private def guideSpeedFor(
    speeds: List[(GuideSpeed, BrightnessConstraints)],
    gMag:   BrightnessValue
  ): Option[GuideSpeed] =
    speeds
      .find(_._2.contains(Band.Gaia, gMag))
      .map(_._1)

  def resultLabel(a: AgsAnalysis): String = a match
    case _: Usable                 => "usable"
    case _: NotReachableAtPosition => "not_reachable"
    case _: VignettesScience       => "vignettes_science"
    case _: NoGuideStarForProbe    => "no_guide_star"
    case _: NoMagnitudeForBand     => "no_magnitude"
    case _: MagnitudeTooFaint      => "magnitude_too_faint"
    case _: MagnitudeTooBright     => "magnitude_too_bright"

  // Runs the analyisis for a single guide star at a single position
  protected def runAnalysis(
    conditions:      ConstraintSet,
    gsOffset:        Offset,
    protectedShapes: List[Shape],
    pos:             OffsetPosition,
    params:          AgsParams,
    gsc:             GuideStarCandidate,
    speeds:          List[(GuideSpeed, BrightnessConstraints)],
    calcs:           NonEmptyMap[OffsetPosition, AgsGeomCalc]
  ): AgsAnalysis = {
    val geoms = calcs.lookup(pos)
    if (!geoms.exists(_.isReachable(gsOffset)))
      // Do we have a g magnitude
      val guideSpeed = gsc.gBrightness.flatMap { case (_, g) => guideSpeedFor(speeds, g) }
      AgsAnalysis.NotReachableAtPosition(pos, params.probe, guideSpeed, gsc)
    else if (geoms.exists(g => protectedShapes.exists(ps => g.overlapsProtectedArea(gsOffset, ps))))
      AgsAnalysis.VignettesScience(gsc, pos)
    else
      magnitudeAnalysis(
        conditions,
        params.probe,
        gsOffset,
        gsc,
        // calculate vignetting
        geoms
          .map(c => (o: Offset) => c.vignettingArea(o))
          .getOrElse((_: Offset) => Area.MaxArea),
        pos
      )(speeds)
  }

  /**
   * Analysis of the suitability of the magnitude of the given guide star regardless of its
   * reachability.
   */
  protected def magnitudeAnalysis(
    constraints:    ConstraintSet,
    guideProbe:     GuideProbe,
    gsOffset:       Offset,
    guideStar:      GuideStarCandidate,
    vignettingArea: Offset => Area,
    position:       OffsetPosition
  )(speeds: List[(GuideSpeed, BrightnessConstraints)]): AgsAnalysis = {

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
             vignettingArea(gsOffset)
      )
    }

    // Do we have a g magnitude
    guideStar.gBrightness match {
      case Some((_, g)) =>
        guideSpeedFor(speeds, g)
          .map(usable)
          .getOrElse(NoGuideStarForProbe(guideProbe, guideStar, position.posAngle))
      case _            => NoMagnitudeForBand(guideProbe, guideStar, position.posAngle)
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
    val guideSpeeds = guideSpeedLimits(constraints, params.probe, wavelength)
    val calcsStart  = System.nanoTime()
    val calcs       = params.posCalculations(positions)
    val calcsNanos  = System.nanoTime() - calcsStart
    val bc          = constraintsFor(guideSpeeds)
    AgsContextBuffer(guideSpeeds, calcs, bc, calcsNanos)
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
    val protectedShapes = params.protectedShapes(noZones)

    in =>
      (in.filter(c =>
         c.gBrightness.exists: (_, g) =>
           ctx.brightnessConstraint.exists(_.contains(Band.Gaia, g))
       ),
       Stream.emits[F, OffsetPosition](positions.toList)
      )
        .mapN { (gsc, position) =>
          val offset = baseCoordinates.diff(gsc.tracking.baseCoordinates).offset
          runAnalysis(
            constraints,
            offset,
            protectedShapes,
            position,
            params,
            gsc,
            ctx.guideSpeeds,
            ctx.calcs
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

    val accepted = candidates.filter: c =>
      c.gBrightness.exists: (_, g) =>
        ctx.brightnessConstraint.exists(_.contains(Band.Gaia, g))

    val sciOffsets      = scienceCoordinates.map(baseCoordinates.diff(_).offset)
    val noZones         = blindOffset
      .map(baseCoordinates.diff(_).offset)
      .fold(sciOffsets)(_ :: sciOffsets)
    val protectedShapes = params.protectedShapes(noZones)

    val anStart  = System.nanoTime()
    val analyses = accepted.flatMap: gsc =>
      val offset = baseCoordinates.diff(gsc.tracking.baseCoordinates).offset

      positions.toList.map: position =>
        runAnalysis(
          constraints,
          offset,
          protectedShapes,
          position,
          params,
          gsc,
          ctx.guideSpeeds,
          ctx.calcs
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

}
