// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.mos

import lucuma.core.enums.Instrument
import lucuma.core.enums.MosDispersionDirection
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.mos.MosMaskHeader
import lucuma.core.model.mos.MosMaskSlit

import scala.math.atan2
import scala.math.cos
import scala.math.sin
import scala.math.sqrt

/**
 * On-sky geometry of a MOS mask design: the slit placement area and every aperture, as offsets
 * from the design's pointing.
 *
 * All shapes come from a single transform, so every aperture of a valid design falls within the
 * outline.
 *
 * @param outline  the slit placement area
 * @param slits    one aperture per input slit, in input order; slits with `Ignore` priority are
 *                 not cut into the mask but still get an aperture here, left to the caller to
 *                 filter
 * @param rotation rotation of the pre-image's pixel axes onto the sky
 */
final case class MosMaskGeometry(
  outline:  ShapeExpression,
  slits:    List[ShapeExpression],
  rotation: Angle
)

object MosMaskGeometry:

  /**
   * The subset of a slit's description that determines its geometry, for callers whose mask data
   * does not come from a parsed file. With a parsed design, use [[fromMask]] directly.
   */
  final case class Slit(
    coordinates:      Coordinates,
    x:                Double,
    y:                Double,
    width:            Angle,
    length:           Angle,
    offsetAlongSlit:  Angle,
    offsetAcrossSlit: Angle,
    tilt:             Angle
  )

  object Slit:
    def fromMaskSlit(s: MosMaskSlit): Slit =
      Slit(
        coordinates = s.coordinates,
        x = s.x,
        y = s.y,
        width = s.slitWidth,
        length = s.slitLength,
        offsetAlongSlit = s.offsetAlongSlit,
        offsetAcrossSlit = s.offsetAcrossSlit,
        tilt = s.tilt
      )

  /** Computes the geometry of a parsed design, if it can be oriented; see [[fromSlits]]. */
  def fromMask(header: MosMaskHeader, slits: List[MosMaskSlit]): Option[MosMaskGeometry] =
    fromSlits(
      header.instrument,
      header.dispersionDirection,
      header.pointing,
      slits.map(Slit.fromMaskSlit)
    )

  /**
   * Computes the geometry of a design, if it can be oriented.
   *
   * A mask file does not record how its detector frame maps onto the sky. It does not need to be
   * assumed either: every slit carries its position in both frames — pre-image pixels and sky
   * coordinates — and together the slits determine the rotation, the plate scale and the pointing
   * pixel. Parity is fixed per instrument, because mask design software rejects pre-images in any
   * other orientation.
   *
   * Yields `None` for a design with fewer than two distinct slits, which cannot be oriented, and
   * for an instrument without a modelled slit placement area.
   */
  def fromSlits(
    instrument:          Instrument,
    dispersionDirection: MosDispersionDirection,
    pointing:            Coordinates,
    slits:               List[Slit]
  ): Option[MosMaskGeometry] =
    for
      config <- instrumentConfig(instrument)
      fit    <- fitTransform(pointing, slits, config.flipped)
    yield build(dispersionDirection, slits, config.vertices, fit)

  /**
   * Per-instrument facts about the pre-image frame: the GMMPS slit placement area, in arcsec in
   * the pre-image's pixel axes, and the frame's parity.
   *
   * The parity encodes the same per-instrument facts GMMPS hardcodes when it derives a mask's
   * position angle (get_OT_posangle.cc), but the booleans are opposite: GMMPS names parity in
   * raw (RA, Dec) CD-matrix axes, where the RA axis runs backwards, while `flipped` here says
   * whether F negates pixel y in the (p, q) offset frame.
   */
  private case class InstrumentConfig(vertices: List[(Int, Int)], flipped: Boolean)

  private def instrumentConfig(instrument: Instrument): Option[InstrumentConfig] =
    instrument match
      case Instrument.GmosNorth  =>
        Some(InstrumentConfig(lucuma.core.geom.gmos.scienceArea.mosVerticesNorth, flipped = false))
      case Instrument.GmosSouth  =>
        Some(InstrumentConfig(lucuma.core.geom.gmos.scienceArea.mosVerticesSouth, flipped = false))
      case Instrument.Flamingos2 =>
        Some(InstrumentConfig(lucuma.core.geom.flamingos2.scienceArea.mosVertices, flipped = true))
      case _                     => None

  /**
   * Similarity transform from pre-image pixels to pointing-relative sky offsets:
   *
   * {{{ sky = scale * R(θ) * F * (pixel - anchor) }}}
   *
   * where `scale` is the plate scale in arcsec per pixel, `R(θ)` the detector-to-sky
   * rotation, `F` a reflection of pixel y when `flipped`, and `anchor` the pixel that lands on
   * the pointing.
   */
  private case class Fit(
    θ:       Double,
    scale:   Double,
    flipped: Boolean,
    anchorX: Double,
    anchorY: Double
  )

  /**
   * Recovers the [[Fit]] from the slits themselves. The slit pattern is drawn twice — once in
   * pre-image pixels, once in sky offsets from the pointing — and the two drawings differ only by
   * the transform being sought: a shift, a rotation and a change of scale. Centering each pattern
   * on its centroid takes the shift out of the problem; rotation and scale then fall out of the
   * per-slit comparisons directly, and the shift is recovered last as the pixel that reconciles
   * the two centroids.
   */
  private def fitTransform(
    pointing: Coordinates,
    slits:    List[Slit],
    flipped:  Boolean
  ): Option[Fit] =
    if slits.sizeIs < 2 then None
    else
      val n = slits.size.toDouble

      val sky = slits.map: s =>
        val o = pointing.diff(s.coordinates).offset
        (o.p.toAngle.toSignedDoubleDecimalArcseconds, o.q.toAngle.toSignedDoubleDecimalArcseconds)

      val pix = slits.map(s => (s.x, s.y))

      val (scx, scy) = (sky.map(_._1).sum / n, sky.map(_._2).sum / n)
      val (pcx, pcy) = (pix.map(_._1).sum / n, pix.map(_._2).sum / n)

      val flip  = if flipped then -1.0 else 1.0
      val pairs = sky.zip(pix).map { case ((p, q), (x, y)) =>
        (p - scx, q - scy, x - pcx, flip * (y - pcy))
      }
      // Centering the two clouds removes the translation, leaving rotation and scale, which
      // have a closed-form least-squares solution: every slit pair votes for the angle between
      // its pixel and sky vectors, weighted by its distance from the centroid.
      val dot   = pairs.map((p, q, x, y) => p * x + q * y).sum
      val cross = pairs.map((p, q, x, y) => x * q - y * p).sum
      val norm  = pairs.map((_, _, x, y) => x * x + y * y).sum
      val scale = sqrt(dot * dot + cross * cross) / norm
      Option.when(norm > 0.0 && scale > 0.0):
        val θ        = atan2(cross, dot)
        val (ct, st) = (cos(θ), sin(θ))
        // anchor: the pixel that lands exactly on the pointing
        val ax       = pcx - (scx * ct + scy * st) / scale
        val ay       = pcy - flip * (-scx * st + scy * ct) / scale
        Fit(θ, scale, flipped, ax, ay)

  private def build(
    dispersionDirection: MosDispersionDirection,
    slits:               List[Slit],
    vertices:            List[(Int, Int)],
    fit:                 Fit
  ): MosMaskGeometry =
    val (ct, st) = (cos(fit.θ), sin(fit.θ))
    val flip     = if fit.flipped then -1.0 else 1.0

    inline def toSky(x: Double, y: Double): (Double, Double) =
      val yy = flip * y
      (x * ct - yy * st, x * st + yy * ct)

    def polygon(points: List[(Double, Double)]): ShapeExpression =
      ShapeExpression.polygonAt(
        points.map { (x, y) =>
          val (p, q) = toSky(x, y)
          (Angle.fromDoubleArcseconds(p).p, Angle.fromDoubleArcseconds(q).q)
        }*
      )

    val outline = polygon(vertices.map((x, y) => (x.toDouble, y.toDouble)))

    val horizontal = dispersionDirection match
      case MosDispersionDirection.Horizontal => true
      case MosDispersionDirection.Vertical   => false

    // Everything below is in the detector frame, in arcsec: slit width lies along the
    // dispersion axis and the across/along offsets displace the slit from its object.
    def slitShape(s: Slit): ShapeExpression =
      val across = s.offsetAcrossSlit.toSignedDoubleDecimalArcseconds
      val along  = s.offsetAlongSlit.toSignedDoubleDecimalArcseconds
      val cx     = fit.scale * (s.x - fit.anchorX) + (if horizontal then across else along)
      val cy     = fit.scale * (s.y - fit.anchorY) + (if horizontal then along else across)
      val hx     = (if horizontal then s.width else s.length).toSignedDoubleDecimalArcseconds / 2.0
      val hy     = (if horizontal then s.length else s.width).toSignedDoubleDecimalArcseconds / 2.0
      val tilt   = s.tilt.toSignedDoubleDegrees.toRadians
      val (c, n) = (cos(tilt), sin(tilt))
      polygon(
        List((hx, hy), (-hx, hy), (-hx, -hy), (hx, -hy)).map { (x, y) =>
          (cx + x * c - y * n, cy + x * n + y * c)
        }
      )

    MosMaskGeometry(
      outline = outline,
      slits = slits.map(slitShape),
      rotation = Angle.fromDoubleRadians(fit.θ)
    )
