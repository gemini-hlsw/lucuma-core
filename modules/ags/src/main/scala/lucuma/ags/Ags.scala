// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.*
import lucuma.ags.AgsAnalysis.*
import lucuma.ags.AgsGuideQuality.*
import lucuma.catalog.BrightnessConstraints
import lucuma.core.enums.Band
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.ImageQuality
import lucuma.core.geom.Area
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet

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
  def runAnalysis(
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
  def magnitudeAnalysis(
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
      def worseOrEqual(iq: ImageQuality) = constraints.imageQuality >= iq

      val quality = guideSpeed match {
        case GuideSpeed.Fast   =>
          DeliversRequestedIq
        case GuideSpeed.Medium =>
          // TODO Review this limit
          if (worseOrEqual(ImageQuality.PointSix)) DeliversRequestedIq
          else PossibleIqDegradation
        case GuideSpeed.Slow   =>
          // TODO Review this limit
          if (worseOrEqual(ImageQuality.PointEight)) DeliversRequestedIq
          // TODO Review this limit
          else if (worseOrEqual(ImageQuality.PointSix)) PossibleIqDegradation
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

  /**
   * FS2 pipe to do analysis of a stream of Candidate Guide Stars The base coordinates and
   * candidates will be PM corrected
   */
  def agsAnalysisStreamPM[F[_]: Concurrent](
    constraints: ConstraintSet,
    wavelength:  Wavelength,
    baseAt:      Instant => Option[Coordinates],
    scienceAt:   List[Instant => Option[Coordinates]],
    positions:   NonEmptyList[AgsPosition],
    params:      AgsParams,
    instant:     Instant
  ): Pipe[F, GuideStarCandidate, AgsAnalysis] = {

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
          val offset         = offsetAt(baseAt, instant, gsc)
          val scienceOffsets = scienceOffsetsAt(scienceAt, instant, gsc)

          offset
            .map { offset =>
              runAnalysis(constraints, offset, scienceOffsets, position, params, gsc)(guideSpeeds,
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
    positions:          NonEmptyList[AgsPosition],
    params:             AgsParams
  ): Pipe[F, GuideStarCandidate, AgsAnalysis] = {

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
          val offset         = baseCoordinates.diff(gsc.tracking.baseCoordinates).offset
          val scienceOffsets = scienceCoordinates.map(_.diff(gsc.tracking.baseCoordinates).offset)
          runAnalysis(constraints, offset, scienceOffsets, position, params, gsc)(guideSpeeds,
                                                                                  calcs
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
   * Do analysis of a list of Candidate Guide Stars. Proper motion is calculated inside if needed
   */
  def agsAnalysisPM(
    constraints: ConstraintSet,
    wavelength:  Wavelength,
    baseAt:      Instant => Option[Coordinates],
    scienceAt:   List[Instant => Option[Coordinates]],
    positions:   NonEmptyList[AgsPosition],
    params:      AgsParams,
    instant:     Instant,
    candidates:  List[GuideStarCandidate]
  ): List[AgsAnalysis] = {
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
        val scienceOffsets = scienceOffsetsAt(scienceAt, instant, gsc)

        positions.toList.map: position =>
          val oOffset = offsetAt(baseAt, instant, gsc)
          oOffset
            .map: offset =>
              runAnalysis(constraints, offset, scienceOffsets, position, params, gsc)(
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
    positions:          NonEmptyList[AgsPosition],
    params:             AgsParams,
    candidates:         List[GuideStarCandidate]
  ): List[AgsAnalysis] = {
    // Cache the limits for different speeds
    val guideSpeeds = guideSpeedLimits(constraints, wavelength)

    // This is essentially a cache of geometries avoiding calculating them
    // over and over again as they don't change for different positions
    val calcs = params.posCalculations(positions)
    // use constraints to calculate all guide speeds
    val bc    = constraintsFor(guideSpeeds)

    candidates
      .filter(c => c.gBrightness.exists { case (_, g) => bc.exists(_.contains(Band.Gaia, g)) })
      .flatMap(gsc =>
        val offset         = baseCoordinates.diff(gsc.tracking.baseCoordinates).offset
        val scienceOffsets = scienceCoordinates.map(_.diff(gsc.tracking.baseCoordinates).offset)

        positions.toList.map { position =>
          runAnalysis(constraints, offset, scienceOffsets, position, params, gsc)(guideSpeeds,
                                                                                  calcs
          )
        }
      )
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
    GuideSpeed.all.find { speed => // assumes the values are sorted fast to slow
      gaiaBrightnessConstraints(constraints, speed, wavelength).contains(Band.Gaia, magnitude)
    }

  /**
   * Calculates brightness limits for each guide speed
   */
  def guideSpeedLimits(
    constraints: ConstraintSet,
    wavelength:  Wavelength
  ): List[(GuideSpeed, BrightnessConstraints)] =
    GuideSpeed.all.map { speed => // assumes the values are sorted fast to slow
      (speed, gaiaBrightnessConstraints(constraints, speed, wavelength))
    }

}
