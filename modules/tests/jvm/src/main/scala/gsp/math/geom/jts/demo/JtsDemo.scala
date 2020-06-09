// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.geom.jts
package demo

import gsp.math.{Angle, Offset}
import gsp.math.geom.ShapeExpression
import gsp.math.geom.jts.jvm.syntax.awt._
import gsp.math.geom.jts.interpreter._
import gsp.math.geom.syntax.shapeexpression._
import gsp.math.syntax.int._

import java.awt.{List => _, _}
import java.awt.event._

import scala.jdk.CollectionConverters._

/**
 * Throwaway demo code to visualize a shape created using `ShapeExpression`s.
 */
object JtsDemo extends Frame("JTS Demo") {

  val posAngle: Angle         =
    145.deg

  val guideStarOffset: Offset =
    Offset.fromAngles(170543999.µas, -24177003.µas)

  val offsetPos: Offset       =
    Offset.fromAngles(60.arcsec, 60.arcsec)

  // Shape to display
  val shapes: List[ShapeExpression] =
    List(
      GmosOiwfsProbeArm.shapeAt(posAngle, guideStarOffset, offsetPos, Offset.Zero, sideLooking = true),
      GmosScienceAreaGeometry.imaging ↗ offsetPos ⟲ posAngle
    )

  // Scale
  val arcsecPerPixel: Double =
    1.0

  val gridSize: Angle =
    50.arcsec

  // Pixel width and height
  val canvasSize: Int = 800

  val hints: Map[RenderingHints.Key, Object] =
    Map(
      RenderingHints.KEY_ANTIALIASING -> RenderingHints.VALUE_ANTIALIAS_ON,
      RenderingHints.KEY_RENDERING    -> RenderingHints.VALUE_RENDER_QUALITY
    )

  object canvas extends Canvas {
    setBackground(Color.lightGray)
    setSize(canvasSize, canvasSize)

    override def paint(g: Graphics): Unit = {
      val halfCanvas = canvasSize / 2
      val g2d = g.asInstanceOf[Graphics2D]
      g2d.setRenderingHints(hints.asJava)
      g2d.setFont(g2d.getFont.deriveFont(8.0f))
      g2d.translate(halfCanvas, halfCanvas)

      // Cross hair showing center.
      g2d.drawLine(-2, 0, 2, 0)
      g2d.drawLine(0, -2, 0, 2)

      // Pixels in each grid square.
      val gpx = Angle.signedArcseconds.get(gridSize).toDouble / arcsecPerPixel

      val origStroke = g2d.getStroke

      // Use a light dotted line for the grid.
      g2d.setStroke(new BasicStroke(0.5f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, Array(1.0f, 3.0f), 0.0f))

      // Draw the grid.
      (0 to (canvasSize.toDouble / gpx).floor.toInt/2).foreach { i =>

        // distance from center in pixels
        val dpx = (i * gpx).round.toInt

        // distance from center in arcsec
        val das = (i * Angle.signedArcseconds.get(gridSize).toDouble).round

        // Draw the labels (p increasing to the left, q increasing upward)
        g2d.drawString(s"$das", -dpx + 5, - halfCanvas + 10)
        if (das != 0L) g2d.drawString(s"-$das", dpx + 5, - halfCanvas + 10)
        g2d.drawString(s"$das", - halfCanvas + 5, -dpx - 5)
        if (das != 0L) g2d.drawString(s"-$das", - halfCanvas + 5, dpx - 5)

        // Draw the grid lines
        g2d.drawLine(-dpx, -halfCanvas, -dpx, halfCanvas)
        g2d.drawLine(dpx, -halfCanvas, dpx, halfCanvas)
        g2d.drawLine(-halfCanvas, -dpx, halfCanvas, -dpx)
        g2d.drawLine(-halfCanvas, dpx, halfCanvas, dpx)
      }
      g2d.setStroke(origStroke)

      // Finally, draw the shape.
      shapes.foreach { shape =>
        shape.eval match {
          case jts: JtsShape => g2d.draw(jts.toAwt(arcsecPerPixel))
          case x             => sys.error(s"Whoa unexpected shape type: $x")
        }
      }

    }
  }

  def main(args: Array[String]): Unit = {
    setSize(canvasSize, canvasSize)

    addWindowListener(new WindowAdapter() {
      override def windowClosing(windowEvent: WindowEvent): Unit = {
        System.exit(0)
      }
    })

    add(BorderLayout.CENTER, canvas)

    setVisible(true)
  }
}
