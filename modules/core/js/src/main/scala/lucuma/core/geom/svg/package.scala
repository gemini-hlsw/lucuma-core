// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import cats._
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.syntax.all._
import lucuma.core.geom.jts.JtsShape
import lucuma.svgdotjs.ArrayXY
import lucuma.svgdotjs.Container
import lucuma.svgdotjs.Element
import lucuma.svgdotjs.PointArray
import lucuma.svgdotjs.SVGArray
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.Polygon

import scala.scalajs.js.JSConverters._

package object svg   {
  type ScalingFn        = Double => Double
  type SvgPostProcessor = Element => Element

  implicit class RenderSvgSyntax[A](val a: A) extends AnyVal {
    def toSvg(base:    Container, pp: SvgPostProcessor = identity, scalingFn: ScalingFn = identity)(
      implicit render: RenderSvg[A]
    ): Container =
      pp(render.toSvg(base, pp, scalingFn, a)).asInstanceOf[Container]
  }

  implicit class CoordinatesCollectionOps[F[_]](val c: F[Coordinate]) extends AnyVal {
    def toSvgPoints(implicit T: Traverse[F]): String = c.map(c => s"${c.x}, ${c.y}").mkString_(" ")
  }

  implicit class CoordinatesArrayOps(val c: Array[Coordinate]) extends AnyVal {
    def toArrayXY(fn: ScalingFn): SVGArray[ArrayXY] =
      new SVGArray(c.map(c => (fn(c.x), fn(c.y)): ArrayXY).toJSArray)
    def toArrayXY: SVGArray[ArrayXY] =
      new SVGArray(c.map(c => (c.x, c.y): ArrayXY).toJSArray)
  }

  val geometryUnionSemigroup: Semigroup[Geometry] =
    Semigroup.instance(_.union(_))
}

package svg {

  /**
    * Typeclass to render an A to svg
    */
  trait RenderSvg[A] {
    def toSvg(base: Container, pp: SvgPostProcessor, scalingFn: ScalingFn, a: A): Container
  }

  object RenderSvg {
    def apply[A: RenderSvg]: RenderSvg[A] = implicitly[RenderSvg[A]]
  }

  object implicits {
    // Basic instancess of typeclasses
    implicit val polygonCollection: RenderSvg[Polygon] =
      new RenderSvg[Polygon] {
        def toSvg(
          base:      Container,
          pp:        SvgPostProcessor,
          scalingFn: ScalingFn,
          a:         Polygon
        ): Container = {
          val pa: PointArray = new PointArray(a.getCoordinates.toArrayXY(scalingFn))
          val polygon        = base.polygon(pa)
          // Polygon is a Container but the svg facade doesn't reflect that
          pp(polygon).asInstanceOf[Container]
        }
      }

    implicit val renderGeometry: RenderSvg[Geometry] =
      new RenderSvg[Geometry] {
        def toSvg(
          base:      Container,
          pp:        SvgPostProcessor,
          scalingFn: ScalingFn,
          a:         Geometry
        ): Container =
          a match {
            case p: Polygon            => p.toSvg(base, pp, scalingFn)
            case p: GeometryCollection => p.toSvg(base, pp, scalingFn)
            case _                     =>
              // We should probably add new typeclasses for other geometries but this will do for now
              throw new IllegalArgumentException("Unrecognized Geometry class: " + a.getClass())
          }
      }

    implicit val renderGeometryCollection: RenderSvg[GeometryCollection] =
      new RenderSvg[GeometryCollection] {
        def toSvg(
          base:      Container,
          pp:        SvgPostProcessor,
          scalingFn: ScalingFn,
          a:         GeometryCollection
        ): Container = {
          val g = base.group()
          a.geometries.foreach(_.toSvg(g, pp, scalingFn))
          pp(g).asInstanceOf[Container]
        }
      }

    implicit val renderJtsShape: RenderSvg[JtsShape] =
      new RenderSvg[JtsShape] {
        def toSvg(
          base:      Container,
          pp:        SvgPostProcessor,
          scalingFn: ScalingFn,
          a:         JtsShape
        ): Container = {
          val svg = a.g.toSvg(base, pp, scalingFn)
          pp(svg).asInstanceOf[Container]
        }
      }

    implicit val renderJtsShapeList: RenderSvg[NonEmptyList[JtsShape]] =
      new RenderSvg[NonEmptyList[JtsShape]] {
        def toSvg(
          base:      Container,
          pp:        SvgPostProcessor,
          scalingFn: ScalingFn,
          a:         NonEmptyList[JtsShape]
        ): Container = {
          // Safari doesn't support transformations on the svg directly, but it can transfor a group below it
          val containerGroup = base.group()
          containerGroup.addClass("jts-root-group")
          // We should calculate the viewbox of the whole geometry
          val composite      = a.map(_.g).reduce(svg.geometryUnionSemigroup)
          a.map(_.g).toList.map(_.toSvg(containerGroup, pp, scalingFn))
          val envelope       = composite.getBoundary.getEnvelopeInternal
          base.viewbox(scalingFn(envelope.getMinX),
                       scalingFn(envelope.getMinY),
                       scalingFn(envelope.getWidth),
                       scalingFn(envelope.getHeight)
          )
          // Note the svg is reversed on y but we'll let clients do the flip
          pp(base).asInstanceOf[Container]
        }
      }

    implicit val renderJtsShapeMapOfIds: RenderSvg[NonEmptyMap[String, JtsShape]] =
      new RenderSvg[NonEmptyMap[String, JtsShape]] {
        def toSvg(
          base:      Container,
          pp:        SvgPostProcessor,
          scalingFn: ScalingFn,
          a:         NonEmptyMap[String, JtsShape]
        ): Container = {
          // Safari doesn't support transformations on the svg directly, but it can transfor a group below it
          val containerGroup = base.group()

          containerGroup.addClass("jts-root-group")
          // We should calculate the viewbox of the whole geometry
          val composite = a.toNonEmptyList.map(_.g).reduce(geometryUnionSemigroup)
          a.toNel.map {
            case (id, g) =>
              val c = g.toSvg(containerGroup, pp, scalingFn)
              // Set an id per geometry
              c.id(id)
          }
          val envelope  = composite.getBoundary.getEnvelopeInternal
          base.viewbox(scalingFn(envelope.getMinX),
                       scalingFn(envelope.getMinY),
                       scalingFn(envelope.getWidth),
                       scalingFn(envelope.getHeight)
          )
          // Note the svg is reversed on y but we'll let clients do the flip
          base
        }
      }

  }
}
