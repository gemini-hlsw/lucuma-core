// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.bench

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import lucuma.ags.*
import lucuma.ags.syntax.*
import lucuma.core.enums.Band
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Offset
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.model.AirMassBound
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.SiderealTracking
import lucuma.core.util.Enumerated

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

/** Everything `Ags.agsAnalysis` takes, so the benchmarks replay a request captured in Explore. */
case class AgsWorkload(
  name:               String,
  constraints:        ConstraintSet,
  wavelength:         Wavelength,
  baseCoordinates:    Coordinates,
  scienceCoordinates: List[Coordinates],
  blindOffset:        Option[Coordinates],
  posAngles:          NonEmptyList[Angle],
  acqOffsets:         Option[AcquisitionOffsets],
  sciOffsets:         Option[ScienceOffsets],
  params:             AgsParams,
  candidates:         List[GuideStarCandidate]
):
  def run(using lucuma.core.geom.ShapeInterpreter): AgsAnalysisResult =
    Ags.agsAnalysis(constraints,
                    wavelength,
                    baseCoordinates,
                    scienceCoordinates,
                    blindOffset,
                    posAngles,
                    acqOffsets,
                    sciOffsets,
                    params,
                    candidates
    )

  def positions: NonEmptyList[lucuma.core.geom.offsets.OffsetPosition] =
    Ags
      .generatePositions(Some(baseCoordinates), blindOffset, posAngles, acqOffsets, sciOffsets)
      .value
      .toNonEmptyList

object AgsWorkload:

  // N offsets on a square grid spanning the captured request's +/-60" dither pattern
  private def grid(n: Int): List[Offset] =
    val k    = math.ceil(math.sqrt(n.toDouble)).toInt
    val step = if k > 1 then 120.0 / (k - 1) else 0.0
    val base = if k > 1 then -60.0 else 0.0
    (for
      i <- 0.until(k)
      j <- 0.until(k)
    yield Offset.signedDecimalArcseconds.reverseGet((base + i * step, base + j * step)))
      .take(n)
      .toList

  /** The captured request with its offsets replaced by an N-point grid, for scaling runs. */
  def withOffsets(n: Int): AgsWorkload =
    real.copy(
      name = n.toString,
      sciOffsets = NonEmptySet.fromSet(SortedSet.from(grid(n).map(_.guided))).map(ScienceOffsets(_))
    )

  /** `real` for the captured Explore request as is, a number N for `withOffsets(N)`. */
  def named(s: String): Option[AgsWorkload] =
    if s == "real" then Some(real) else s.toIntOption.filter(_ > 0).map(withOffsets)

  /** Parses the text printed by Explore's throwaway `AgsDump` (angles and offsets in µas). */
  def parse(name: String, text: String): AgsWorkload =
    val rows                              = text.linesIterator.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).toList
    def values(key: String): List[String] = rows.collect { case s"$k $v" if k == key => v }
    def one(key: String): String          =
      values(key).headOption.getOrElse(sys.error(s"$name: missing '$key'"))

    def tag[A: Enumerated](s: String): A                        =
      Enumerated[A].fromTag(s).getOrElse(sys.error(s"$name: unknown tag '$s'"))
    def angle(s: String): Angle                                 = Angle.fromMicroarcseconds(s.toLong)
    def coords(s: String): Coordinates                          = s match
      case s"$ra $dec" =>
        Coordinates(RightAscension.fromAngleExact.getOption(angle(ra)).get,
                    Declination.fromAngle.getOption(angle(dec)).get
        )
    def offset(s: String): GuidedOffset                         = s match
      case s"$p $q" => Offset(Offset.P(angle(p)), Offset.Q(angle(q))).guided
    def offsets(key: String): Option[NonEmptySet[GuidedOffset]] =
      NonEmptySet.fromSet(SortedSet.from(values(key).map(offset)))

    val constraints = one("constraints") match
      case s"$iq $cc $sb $wv ByAirMass($min,$max)" =>
        ConstraintSet(
          tag[ImageQuality.Preset](iq),
          tag[CloudExtinction.Preset](cc),
          tag[SkyBackground](sb),
          tag[WaterVapor](wv),
          ElevationRange.ByAirMass(AirMassBound.unsafeFromBigDecimal(BigDecimal(min)),
                                   AirMassBound.unsafeFromBigDecimal(BigDecimal(max))
          )
        )
      case other                                   => sys.error(s"$name: constraints '$other'")

    val params: AgsParams = one("params") match
      case s"GmosImaging($port,GmosOIWFS)" => AgsParams.GmosImaging(tag[PortDisposition](port))
      case s"GmosImaging($port,PWFS1)"     =>
        AgsParams.GmosImaging(tag[PortDisposition](port)).withPWFS1
      case s"GmosImaging($port,PWFS2)"     =>
        AgsParams.GmosImaging(tag[PortDisposition](port)).withPWFS2
      case other                           => sys.error(s"$name: unsupported params '$other'")

    val candidates = values("cand").map:
      case s"$id $ra $dec $mags" =>
        val bs = mags
          .split(",")
          .toList
          .map:
            case s"$b=$v" => tag[Band](b) -> BrightnessValue.unsafeFrom(BigDecimal(v))
        GuideStarCandidate(id.toLong,
                           SiderealTracking.const(coords(s"$ra $dec")),
                           SortedMap.from(bs)
        )

    AgsWorkload(
      name,
      constraints,
      Wavelength.intPicometers.getOption(one("wavelength").toInt).get,
      coords(one("base")),
      values("science").map(coords),
      values("blind").headOption.map(coords),
      NonEmptyList.fromListUnsafe(values("posAngle").map(angle)),
      offsets("acq").map(AcquisitionOffsets(_)),
      offsets("sci").map(ScienceOffsets(_)),
      params,
      candidates
    )

  /** GMOS imaging, PWFS1, 7x7 dither grid (50 offsets), 121 Gaia candidates, from Explore. */
  lazy val real: AgsWorkload = parse("real", AgsRealFixture.text)
