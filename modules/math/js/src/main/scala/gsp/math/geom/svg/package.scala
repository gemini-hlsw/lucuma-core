// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.geom

import scala.scalajs.js.JSConverters._

import cats._
import cats.implicits._
import gpp.svgdotjs.svgdotjsSvgJs.mod.ArrayXY
import gpp.svgdotjs.svgdotjsSvgJs.mod.Container
import gpp.svgdotjs.svgdotjsSvgJs.mod.PointArray
import gpp.svgdotjs.svgdotjsSvgJs.mod.{ Array => SVGArray }
import gsp.math.geom.jts.JtsShape
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.Polygon

package object svg   {
  implicit class RenderSvgSyntax[A](val a: A) extends AnyVal {
    def toSvg(base: Container)(implicit render: RenderSvg[A]): Container = render.toSvg(base, a)
  }

  implicit class CoordinatesCollectionOps[F[_]](val c: F[Coordinate]) extends AnyVal {
    def toSvgPoints(implicit T: Traverse[F]): String = c.map(c => s"${c.x}, ${c.y}").mkString_(" ")
  }

  implicit class CoordinatesArrayOps(val c: Array[Coordinate]) extends AnyVal {
    def toArrayXY: SVGArray[ArrayXY] =
      new SVGArray(c.map(c => (c.x, c.y): ArrayXY).toJSArray)
  }
}

package svg {

  /**
    * Typeclass to render an A to svg
    */
  trait RenderSvg[A] {
    def toSvg(base: Container, a: A): Container
  }

  object RenderSvg {
    def apply[A: RenderSvg]: RenderSvg[A] = implicitly[RenderSvg[A]]
  }

  object implicits {
    // Basic instancess of typeclasses
    implicit val polygonCollection: RenderSvg[Polygon] =
      new RenderSvg[Polygon] {
        def toSvg(base: Container, a: Polygon): Container = {
          val pa: PointArray = new PointArray(a.getCoordinates.toArrayXY)
          val polygon        = base.polygon(pa)
          polygon.addClass("jts-polygon")
          polygon.asInstanceOf[Container]
        }
      }

    implicit val renderGeometry: RenderSvg[Geometry] =
      new RenderSvg[Geometry] {
        def toSvg(base: Container, a: Geometry): Container =
          a match {
            case p: Polygon            => p.toSvg(base)
            case p: GeometryCollection => p.toSvg(base)
            case _                     =>
              // We should probably add new typeclasses for other geometries but this will do for now
              throw new IllegalArgumentException("Unrecognized Geometry class: " + a.getClass())
          }
      }

    implicit val renderGeometryCollection: RenderSvg[GeometryCollection] =
      new RenderSvg[GeometryCollection] {
        def toSvg(base: Container, a: GeometryCollection): Container = {
          val g = base.group()
          a.geometries.foreach(_.toSvg(g))
          g.addClass("jts-group")
          g
        }
      }

    implicit val renderJtsShape: RenderSvg[JtsShape] =
      new RenderSvg[JtsShape] {
        def toSvg(base: Container, a: JtsShape): Container = {
          val svg      = a.g.toSvg(base)
          val envelope = a.g.getBoundary.getEnvelopeInternal
          base.viewbox(envelope.getMinX, envelope.getMinY, envelope.getWidth, envelope.getHeight)
          svg.addClass("jts")
          svg
        }
      }
  }
}
