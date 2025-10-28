// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.effect.Concurrent
import cats.syntax.all.*
import cats.syntax.semigroup.*
import fs2.*
import lucuma.ags.AgsAnalysis.*
import lucuma.ags.AgsGuideQuality.*
import lucuma.catalog.BrightnessConstraints
import lucuma.core.enums.Band
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.GuideSpeed
import lucuma.core.geom.Area
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ImageQuality

import java.time.Instant

object Ags {

  private def guideSpeedFor(
    speeds: List[(GuideSpeed, BrightnessConstraints)],
    gMag:   BrightnessValue
  ): Option[GuideSpeed] =
    speeds
      .find(_._2.contains(Band.Gaia, gMag))
      .map(_._1)

  // Runs the analyisis for a single guide star at a single position
  protected def runAnalysis(
    conditions:     ConstraintSet,
    gsOffset:       Offset,
    scienceOffsets: List[Offset],
    pos:            AgsPosition,
    params:         AgsParams,
    gsc:            GuideStarCandidate
  )(
    speeds:         List[(GuideSpeed, BrightnessConstraints)],
    calcs:          NonEmptyMap[AgsPosition, AgsGeomCalc]
  ): AgsAnalysis = {
    val geoms = calcs.lookup(pos)
    if (!geoms.exists(_.isReachable(gsOffset)))
      // Do we have a g magnitude
      val guideSpeed = gsc.gBrightness.flatMap { case (_, g) => guideSpeedFor(speeds, g) }
      AgsAnalysis.NotReachableAtPosition(pos, params.probe, guideSpeed, gsc)
    else if (geoms.exists(g => scienceOffsets.exists(g.overlapsScience(_))))
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
    position:       AgsPosition
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

  private def offsetAt(
    at:      Instant => Option[Coordinates],
    instant: Instant,
    gsc:     GuideStarCandidate
  ): Option[Offset] =
    (at(instant), gsc.tracking.at(instant)).mapN(_.diff(_).offset)

  private def scienceOffsetsAt(
    scienceAt: List[Instant => Option[Coordinates]],
    instant:   Instant,
    gsc:       GuideStarCandidate
  ): List[Offset] =
    scienceAt.map(s => offsetAt(s, instant, gsc)).flatten

  private def buildPositions(
    anglesToTest:   NonEmptyList[Angle],
    acqOffsets:     Option[NonEmptyList[Offset]],
    scienceOffsets: Option[NonEmptyList[Offset]]
  ): NonEmptyList[AgsPosition] =
    val allOffsets =
      (acqOffsets, scienceOffsets) match
        case (Some(a), Some(s))  => (a |+| s).some
        case (a @ Some(_), None) => a
        case (None, s @ Some(_)) => s
        case _                   => none

    allOffsets.fold(anglesToTest.map(AgsPosition(_, Offset.Zero))): offsets =>
      for {
        pa  <- anglesToTest
        off <- offsets.distinct
      } yield AgsPosition(pa, off)

  private def generatePositions(
    baseCoordinates: Coordinates,
    blindOffset:     Option[Coordinates],
    posAngles:       NonEmptyList[Angle],
    acqOffsets:      Option[NonEmptyList[Offset]],
    scienceOffsets:  Option[NonEmptyList[Offset]]
  ): NonEmptyList[AgsPosition] = {
    val acqOffsetsOnBlind = (blindOffset, acqOffsets) match
      case (Some(blind), Some(acqOffsets)) =>
        // Not a typo, it is the offset to the blind offset
        val blindOffsetOffset = blind.diff(baseCoordinates).offset
        acqOffsets.map(_ + blindOffsetOffset).some
      case (Some(_), None)                 =>
        none
      case (None, acqOffsets)              =>
        acqOffsets

    buildPositions(posAngles, acqOffsetsOnBlind, scienceOffsets)
  }

  private def generatePositions(
    baseAt:             Instant => Option[Coordinates],
    blindOffset:        Option[Instant => Option[Coordinates]],
    posAngles:          NonEmptyList[Angle],
    acquisitionOffsets: Option[NonEmptyList[Offset]],
    scienceOffsets:     Option[NonEmptyList[Offset]],
    instant:            Instant
  ): NonEmptyList[AgsPosition] = {
    val acqOffsetsOnBlind = (blindOffset, acquisitionOffsets) match
      case (Some(blindAt), Some(acqOffsets)) =>
        (baseAt(instant), blindAt(instant))
          .mapN((base, blind) =>
            val offset = blind.diff(base).offset
            acqOffsets.map(_ + offset)
          )
          .getOrElse(acqOffsets)
          .some
      case (Some(_), None)                   =>
        none
      case (None, acqOffsets)                =>
        acqOffsets

    buildPositions(posAngles, acqOffsetsOnBlind, scienceOffsets)
  }

  /**
   * FS2 pipe to do analysis of a stream of Candidate Guide Stars The base coordinates and
   * candidates will be PM corrected
   */
  protected def agsAnalysisStreamPM[F[_]: Concurrent](
    constraints:        ConstraintSet,
    wavelength:         Wavelength,
    baseAt:             Instant => Option[Coordinates],
    scienceAt:          List[Instant => Option[Coordinates]],
    blindOffset:        Option[Instant => Option[Coordinates]],
    posAngles:          NonEmptyList[Angle],
    acquisitionOffsets: Option[NonEmptyList[Offset]],
    scienceOffsets:     Option[NonEmptyList[Offset]],
    params:             AgsParams,
    instant:            Instant
  ): Pipe[F, GuideStarCandidate, AgsAnalysis] = {
    val positions =
      generatePositions(baseAt, blindOffset, posAngles, acquisitionOffsets, scienceOffsets, instant)

    // Cache the limits for different speeds
    val guideSpeeds = guideSpeedLimits(constraints, wavelength)
    // This is essentially a cache of geometries avoiding calculatting them
    // over and over again as they don't change for different positions
    val calcs       = params.posCalculations(positions)
    // use constraints to calculate all guide speeds
    val bc          = constraintsFor(guideSpeeds)

    in =>
      (in.filter(c => c.gBrightness.exists { case (_, g) => bc.exists(_.contains(Band.Gaia, g)) }),
       Stream.emits[F, AgsPosition](positions.toList).repeat
      )
        .mapN { case (gsc, position) =>
          val offset     = offsetAt(baseAt, instant, gsc)
          val sciOffsets = scienceOffsetsAt(scienceAt, instant, gsc)

          offset
            .map { offset =>
              runAnalysis(constraints, offset, sciOffsets, position, params, gsc)(guideSpeeds,
                                                                                  calcs
              )
            }
            .getOrElse(ProperMotionNotAvailable(gsc, position.posAngle))
        }
  }

  /**
   * FS2 pipe to do analysis of a stream of Candidate Guide Stars This method assumes the base and
   * candidates are pm corrected already
   */
  def agsAnalysisStream[F[_]](
    constraints:        ConstraintSet,
    wavelength:         Wavelength,
    baseCoordinates:    Coordinates,
    scienceCoordinates: List[Coordinates],
    blindOffset:        Option[Coordinates],
    posAngles:          NonEmptyList[Angle],
    acquisitionOffsets: Option[NonEmptyList[Offset]],
    scienceOffsets:     Option[NonEmptyList[Offset]],
    params:             AgsParams
  ): Pipe[F, GuideStarCandidate, AgsAnalysis] = {
    val positions =
      generatePositions(baseCoordinates, blindOffset, posAngles, acquisitionOffsets, scienceOffsets)

    // Cache the limits for different speeds
    val guideSpeeds = guideSpeedLimits(constraints, wavelength)
    // This is essentially a cache of geometries avoiding calculatting them
    // over and over again as they don't change for different positions
    val calcs       = params.posCalculations(positions)
    // use constraints to calculate all guide speeds
    val bc          = constraintsFor(guideSpeeds)

    in =>
      (in.filter(c => c.gBrightness.exists { case (_, g) => bc.exists(_.contains(Band.Gaia, g)) }),
       Stream.emits[F, AgsPosition](positions.toList)
      )
        .mapN { (gsc, position) =>
          val offset     = baseCoordinates.diff(gsc.tracking.baseCoordinates).offset
          val sciOffsets = scienceCoordinates.map(_.diff(gsc.tracking.baseCoordinates).offset)
          runAnalysis(constraints, offset, sciOffsets, position, params, gsc)(guideSpeeds, calcs)
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
   * Do analysis of a list of Candidate Guide Stars. Proper motion is calculated inside if needed
   */
  protected def agsAnalysisPM(
    constraints:        ConstraintSet,
    wavelength:         Wavelength,
    baseAt:             Instant => Option[Coordinates],
    scienceAt:          List[Instant => Option[Coordinates]],
    blindOffset:        Option[Instant => Option[Coordinates]],
    posAngles:          NonEmptyList[Angle],
    acquisitionOffsets: Option[NonEmptyList[Offset]],
    scienceOffsets:     Option[NonEmptyList[Offset]],
    params:             AgsParams,
    instant:            Instant,
    candidates:         List[GuideStarCandidate]
  ): List[AgsAnalysis] = {
    val positions =
      generatePositions(baseAt, blindOffset, posAngles, acquisitionOffsets, scienceOffsets, instant)

    // Cache the limits for different speeds
    val guideSpeeds = guideSpeedLimits(constraints, wavelength)
    // This is essentially a cache of geometries avoiding calculatting them
    // over and over again as they don't change for different positions
    val calcs       = params.posCalculations(positions)
    // use constraints to calculate all guide speeds
    val bc          = constraintsFor(guideSpeeds)

    candidates
      .filter(c => c.gBrightness.exists { case (_, g) => bc.exists(_.contains(Band.Gaia, g)) })
      .flatMap: gsc =>
        val sciOffsets = scienceOffsetsAt(scienceAt, instant, gsc)

        positions.toList.map: position =>
          val oOffset = offsetAt(baseAt, instant, gsc)
          oOffset
            .map: offset =>
              runAnalysis(constraints, offset, sciOffsets, position, params, gsc)(
                guideSpeeds,
                calcs
              )
            .getOrElse(ProperMotionNotAvailable(gsc, position.posAngle))
  }

  /**
   * Do analysis of a list of Candidate Guide Stars Note the base coordinates should be pm corrected
   * if needed.
   *
   * The results contaains every possible combination of candidates and positions, the result will
   * be less than candidates..length * positions.length as some candidates will be filtered out
   */
  def agsAnalysis(
    constraints:        ConstraintSet,
    wavelength:         Wavelength,
    baseCoordinates:    Coordinates,
    scienceCoordinates: List[Coordinates],
    blindOffset:        Option[Coordinates],
    posAngles:          NonEmptyList[Angle],
    acquisitionOffsets: Option[NonEmptyList[Offset]],
    scienceOffsets:     Option[NonEmptyList[Offset]],
    params:             AgsParams,
    candidates:         List[GuideStarCandidate]
  ): List[AgsAnalysis] = {
    val positions =
      generatePositions(baseCoordinates, blindOffset, posAngles, acquisitionOffsets, scienceOffsets)

    // Cache the limits for different speeds
    val guideSpeeds = guideSpeedLimits(constraints, wavelength)

    // This is essentially a cache of geometries avoiding calculating them
    // over and over again as they don't change for different positions
    val calcs = params.posCalculations(positions)
    // use constraints to calculate all guide speeds
    val bc    = constraintsFor(guideSpeeds)

    candidates
      .filter: c =>
        c.gBrightness.exists { case (_, g) => bc.exists(_.contains(Band.Gaia, g)) }
      .flatMap: gsc =>
        val offset     = baseCoordinates.diff(gsc.tracking.baseCoordinates).offset
        val sciOffsets = scienceCoordinates.map(_.diff(gsc.tracking.baseCoordinates).offset)

        positions.toList.map: position =>
          runAnalysis(constraints, offset, sciOffsets, position, params, gsc)(guideSpeeds, calcs)
  }

  /**
   * Determines the fastest possible guide speed (if any) that may be used for guiding given a star
   * with the indicated magnitude.
   */
  def fastestGuideSpeed(
    constraints: ConstraintSet,
    wavelength:  Wavelength,
    magnitude:   BrightnessValue
  ): Option[GuideSpeed] =
    GuideSpeed.inSpeedOrder.find: speed => // assumes the values are sorted fast to slow
      gaiaBrightnessConstraints(constraints, speed, wavelength).contains(Band.Gaia, magnitude)

  /**
   * Calculates brightness limits for each guide speed
   */
  def guideSpeedLimits(
    constraints: ConstraintSet,
    wavelength:  Wavelength
  ): List[(GuideSpeed, BrightnessConstraints)] =
    GuideSpeed.inSpeedOrder.map: speed =>
      (speed, gaiaBrightnessConstraints(constraints, speed, wavelength))

}
