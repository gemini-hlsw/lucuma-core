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
 * On-sky geometry of a MOS mask design including the slit placement area and every aperture, as
 * offsets from the design's pointing.
 *
 * @param outline the slit placement area
 * @param slits   one aperture per input slit, in input order; Ignored apertures of slits are
 *                included and left to the caller to filter.
 * @param rotation detector-to-sky rotation of the pre-image
 */
final case class MosMaskGeometry(
  outline:  ShapeExpression,
  slits:    List[ShapeExpression],
  rotation: Angle
)

object MosMaskGeometry:

  /**
   * The subset of a slit's description that determines its geometry, for callers that hold mask
   * data in another shape than the file model — a GraphQL response, say.
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
   * The mapping from the design's detector frame onto the sky is not recorded in a mask file and
   * is not fixed per instrument — pre-image parity and rotation vary — so it is fitted from the
   * design: every slit carries both its pre-image pixel position and its sky coordinates, which
   * together determine the rotation, plate scale, parity and pointing pixel. Fitting needs at
   * least two slits at distinct positions; a design with fewer cannot be oriented and yields
   * `None`, as does an instrument without a modelled slit placement area.
   */
  def fromSlits(
    instrument:          Instrument,
    dispersionDirection: MosDispersionDirection,
    pointing:            Coordinates,
    slits:               List[Slit]
  ): Option[MosMaskGeometry] =
    for
      vertices <- placementVertices(instrument)
      fit      <- fitTransform(pointing, slits)
    yield build(dispersionDirection, slits, vertices, fit)

  /** The GMMPS slit placement area, in arcsec in the pre-image's pixel axes. */
  private def placementVertices(instrument: Instrument): Option[List[(Int, Int)]] =
    instrument match
      case Instrument.GmosNorth  => Some(lucuma.core.geom.gmos.scienceArea.mosVerticesNorth)
      case Instrument.GmosSouth  => Some(lucuma.core.geom.gmos.scienceArea.mosVerticesSouth)
      case Instrument.Flamingos2 => Some(lucuma.core.geom.flamingos2.scienceArea.mosVertices)
      case _                     => None

  /**
   * Similarity transform from pre-image pixels to pointing-relative sky offsets:
   * `sky = scale * R(theta) * F * (pixel - anchor)`, where `F` reflects pixel y when
   * `flipped`. Solved in closed form (orthogonal Procrustes), trying both parities and
   * keeping the one with the smaller residual.
   */
  private case class Fit(
    theta:   Double,
    scale:   Double,
    flipped: Boolean,
    anchorX: Double,
    anchorY: Double
  )

  private def fitTransform(pointing: Coordinates, slits: List[Slit]): Option[Fit] =
    if slits.sizeIs < 2 then None
    else
      val n = slits.size.toDouble

      val sky = slits.map { s =>
        val o = pointing.diff(s.coordinates).offset
        (arcsec(o.p.toAngle), arcsec(o.q.toAngle))
      }
      val pix = slits.map(s => (s.x, s.y))

      val (scx, scy) = (sky.map(_._1).sum / n, sky.map(_._2).sum / n)
      val (pcx, pcy) = (pix.map(_._1).sum / n, pix.map(_._2).sum / n)

      def solve(flipped: Boolean): Option[(Fit, Double)] =
        val f     = if flipped then -1.0 else 1.0
        val pairs = sky.zip(pix).map { case ((p, q), (x, y)) =>
          (p - scx, q - scy, x - pcx, f * (y - pcy))
        }
        val dot   = pairs.map((p, q, x, y) => p * x + q * y).sum
        val cross = pairs.map((p, q, x, y) => x * q - y * p).sum
        val norm  = pairs.map((_, _, x, y) => x * x + y * y).sum
        val scale = sqrt(dot * dot + cross * cross) / norm
        Option.when(norm > 0.0 && scale > 0.0):
          val theta    = atan2(cross, dot)
          val (ct, st) = (cos(theta), sin(theta))
          val residual = pairs.map { (p, q, x, y) =>
            val rp = scale * (x * ct - y * st) - p
            val rq = scale * (x * st + y * ct) - q
            rp * rp + rq * rq
          }.sum
          // anchor: the pixel that lands exactly on the pointing
          val ax = pcx - (scx * ct + scy * st) / scale
          val ay = pcy - f * (-scx * st + scy * ct) / scale
          (Fit(theta, scale, flipped, ax, ay), residual)

      List(solve(false), solve(true)).flatten.minByOption(_._2).map(_._1)

  private def build(
    dispersionDirection: MosDispersionDirection,
    slits:               List[Slit],
    vertices:            List[(Int, Int)],
    fit:                 Fit
  ): MosMaskGeometry =
    val (ct, st) = (cos(fit.theta), sin(fit.theta))
    val f        = if fit.flipped then -1.0 else 1.0

    def toSky(x: Double, y: Double): (Double, Double) =
      val yy = f * y
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
      val across = arcsec(s.offsetAcrossSlit)
      val along  = arcsec(s.offsetAlongSlit)
      val cx     = fit.scale * (s.x - fit.anchorX) + (if horizontal then across else along)
      val cy     = fit.scale * (s.y - fit.anchorY) + (if horizontal then along else across)
      val hx     = arcsec(if horizontal then s.width else s.length) / 2.0
      val hy     = arcsec(if horizontal then s.length else s.width) / 2.0
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
      rotation = Angle.fromDoubleRadians(fit.theta)
    )

  private def arcsec(a: Angle): Double =
    Angle.signedDecimalArcseconds.get(a).toDouble
