// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Eq
import cats.Order
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import lucuma.catalog.BandsList
import lucuma.core.enums.Band
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.GuideSpeed
import lucuma.core.geom.Area
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue

sealed trait AgsAnalysis derives Eq {
  def quality: AgsGuideQuality = AgsGuideQuality.Unusable
  def isUsable: Boolean        = quality =!= AgsGuideQuality.Unusable
  def target: GuideStarCandidate
  def posAngle: Angle
  def message(withProbe: Boolean): String
}

object AgsAnalysis {

  case class ProperMotionNotAvailable(target: GuideStarCandidate, posAngle: Angle)
      extends AgsAnalysis derives Eq {
    override def message(withProbe: Boolean): String =
      "Cannot calculate proper motion."
  }

  case class VignettesScience(target: GuideStarCandidate, position: AgsPosition) extends AgsAnalysis
      derives Eq {
    val posAngle: Angle = position.posAngle

    override def message(withProbe: Boolean): String =
      "The target overlaps with the science target"
  }

  case class NoGuideStarForProbe(
    guideProbe: GuideProbe,
    target:     GuideStarCandidate,
    posAngle:   Angle
  ) extends AgsAnalysis derives Eq {
    override def message(withProbe: Boolean): String = {
      val p = if (withProbe) s"$guideProbe " else ""
      s"No ${p}guide star selected."
    }
  }

  case class MagnitudeTooFaint(
    guideProbe:     GuideProbe,
    target:         GuideStarCandidate,
    posAngle:       Angle,
    showGuideSpeed: Boolean
  ) extends AgsAnalysis derives Eq {
    override def message(withProbe: Boolean): String = {
      val p  = if (withProbe) s"use $guideProbe" else "guide"
      val gs = if (showGuideSpeed) ", even using the slowest guide speed" else ""
      s"Cannot $p with the star in these conditions$gs."
    }
  }

  case class MagnitudeTooBright(
    guideProbe: GuideProbe,
    target:     GuideStarCandidate,
    posAngle:   Angle
  ) extends AgsAnalysis derives Eq {
    override def message(withProbe: Boolean): String = {
      val p = if (withProbe) s"$guideProbe g" else "G"
      s"${p}uide star is too bright to guide."
    }
  }

  case class NotReachableAtPosition(
    position:   AgsPosition,
    guideProbe: GuideProbe,
    guideSpeed: Option[GuideSpeed],
    target:     GuideStarCandidate
  ) extends AgsAnalysis derives Eq {
    val posAngle: Angle = position.posAngle

    override def message(withProbe: Boolean): String = {
      val p = if (withProbe) s"with ${guideProbe} " else ""
      s"The star is not reachable ${p}at $position."
    }
  }

  case class NoMagnitudeForBand(
    guideProbe: GuideProbe,
    target:     GuideStarCandidate,
    posAngle:   Angle
  ) extends AgsAnalysis derives Eq {
    private val probeBands: List[Band]               = BandsList.GaiaBandsList.bands
    override def message(withProbe: Boolean): String = {
      val p = if (withProbe) s"${guideProbe} g" else "G"
      if (probeBands.length == 1) {
        s"${p}uide star ${probeBands.head}-band magnitude is missing. Cannot determine guiding performance."
      } else {
        s"${p}uide star ${probeBands.map(_.shortName).mkString(", ")}-band magnitudes are missing. Cannot determine guiding performance."
      }
    }
  }

  case class Usable(
    guideProbe:           GuideProbe,
    target:               GuideStarCandidate,
    guideSpeed:           GuideSpeed,
    override val quality: AgsGuideQuality,
    posAngle:             Angle,
    vignetting:           Area
  ) extends AgsAnalysis derives Eq {
    override def message(withProbe: Boolean): String = {
      val qualityMessage = quality match {
        case AgsGuideQuality.DeliversRequestedIq => ""
        case _                                   => s"${quality.message} "
      }
      val p              = if (withProbe) s"${guideProbe} " else ""
      val gs             = s"Guide Speed: ${guideSpeed.toString()}"
      s"$qualityMessage$p$gs. vignetting: ${vignetting.toMicroarcsecondsSquared} µas^2"
      s"$p $quality $gs. vignetting: ${vignetting.toMicroarcsecondsSquared} µas^2"
    }
  }

  object Usable {
    val rankOrder: Order[Usable] =
      Order
        .by(u =>
          (u.guideSpeed,
           u.quality,
           u.vignetting.toMicroarcsecondsSquared,
           u.target.gBrightness,
           u.target.id
          )
        )

    val rankOrdering: Ordering[Usable] = rankOrder.toOrdering
  }

  extension (analysis: Option[AgsAnalysis]) def posAngle: Option[Angle] = analysis.map(_.posAngle)

  extension (results: List[AgsAnalysis])
    // format: off
    /**
     * This method will sort the analysis for quality and internally for positions that give the
     * lowest vignetting.
     *
     * The rules are:
     *   1. Foreach target, collect all the analyses
     *   2. Then group by position angle
     *   3. If any analysis withing a posAngle group is unusable, discard the posAngle because it
     *      means it is not usable at at least one offset.
     *   4. Otherwise, keep the posAngle within the group with the highest sorting - this is the
     *      worst case for the posAngle.
     * 5. For the target, choose the lowest sorting of the worst cases from step 4.
     * NOTE: For any given target, all Usable instances should be the same except vignetting and 
     *       posAngle, so the comparisons really probably boil down to a comparison of vignetting.
     * NOTE: No effort is made to choose a specific angle among angles with equal sorting value.
     */
    // format: on
    def sortUsablePositions: List[Usable] = {
      def chooseForTarget(analysesForTarget: List[AgsAnalysis]): Option[Usable] =
        val forAngles = analysesForTarget
          .groupBy(_.posAngle)
          .toList
          .map { (_, analyses) =>
            chooseForAngle(analyses)
          }
          .flatten
        NonEmptyList.fromList(forAngles).map(_.minimum(Usable.rankOrder))

      def chooseForAngle(analysesForAngle: List[AgsAnalysis]): Option[Usable] =
        analysesForAngle
          .traverse {
            case a: Usable => a.some
            case _         => none
          }
          .flatMap(l =>
            // worst case for this posAngle
            NonEmptyList.fromList(l).map(_.maximum(Usable.rankOrder))
          )

      val usablePerTarget: List[Usable] =
        results
          .groupBy(_.target)
          .toList
          .map { case (_, analyses) =>
            chooseForTarget(analyses)
          }
          .flatten

      usablePerTarget.sorted(Usable.rankOrdering)
    }

}
