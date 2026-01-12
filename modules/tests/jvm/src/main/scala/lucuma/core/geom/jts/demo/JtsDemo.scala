// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.jts
package demo

import cats.syntax.option.*
import lucuma.ags.GeometryType
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.jts.jvm.syntax.awt.*
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask

import java.awt.event.*
import java.awt.{List as _, *}
import scala.jdk.CollectionConverters.*

case class ColoredShape(
  shape: ShapeExpression,
  color: Color,
  stroke: Option[BasicStroke] = None,
  geometryType: Option[GeometryType] = None
)

trait InstrumentShapes:
  def shapes: List[ShapeExpression]
  def coloredShapes: List[ColoredShape] = Nil

trait GmosLSShapes extends InstrumentShapes:
  import lucuma.core.geom.gmos.*

  val posAngle: Angle =
    145.deg

  val guideStarOffset: Offset =
    Offset(170543999.µas.p, -24177003.µas.q)

  val offsetPos: Offset =
    Offset(-60.arcsec.p, 60.arcsec.q)

  val fpu: Option[Either[GmosNorthFpu, GmosSouthFpu]] =
    Some(Right(GmosSouthFpu.LongSlit_5_00)) // None will render imaging detector

  val port: PortDisposition =
    PortDisposition.Side

  // Shape to display
  def shapes: List[ShapeExpression] =
    List(
      probeArm.shapeAt(posAngle, guideStarOffset, offsetPos, fpu, port),
      patrolField.patrolFieldAt(posAngle, offsetPos, fpu, port),
      scienceArea.shapeAt(posAngle, offsetPos, fpu),
      candidatesArea.candidatesAreaAt(posAngle, offsetPos)
    )

trait GmosImagingShapes extends InstrumentShapes:
  import lucuma.core.geom.gmos.*

  val posAngle: Angle =
    145.deg

  val guideStarOffset: Offset =
    Offset(170543999.µas.p, -24177003.µas.q)

  val offsetPos: Offset =
    Offset(-60.arcsec.p, 60.arcsec.q)

  val port: PortDisposition =
    PortDisposition.Side

  // Shape to display
  def shapes: List[ShapeExpression] =
    List(
      probeArm.shapeAt(posAngle, guideStarOffset, offsetPos, none, port),
      patrolField.patrolFieldAt(posAngle, offsetPos, none, port),
      scienceArea.shapeAt(posAngle, offsetPos, none),
      candidatesArea.candidatesAreaAt(posAngle, offsetPos)
    )

trait Flamingos2LSShapes extends InstrumentShapes:
  import lucuma.core.geom.flamingos2.*

  val posAngle: Angle =
    145.deg

  val guideStarOffset: Offset =
    Offset(170543999.µas.p, -24177003.µas.q)

  val offsetPos: Offset =
    Offset(-60.arcsec.p, 60.arcsec.q)

  val fpu: Flamingos2FpuMask = Flamingos2FpuMask.Builtin(Flamingos2Fpu.LongSlit8)
  val lyot: Flamingos2LyotWheel = Flamingos2LyotWheel.F16

  val port: PortDisposition =
    PortDisposition.Bottom

  def shapes: List[ShapeExpression] =
    List(
      ShapeExpression.centeredRectangle(1.arcsec, 1.arcsec).translate(guideStarOffset), // guide star
      probeArm.shapeAt(posAngle, guideStarOffset, offsetPos, lyot, port),
      patrolField.patrolFieldAt(posAngle, offsetPos, lyot, port),
      scienceArea.shapeAt(posAngle, offsetPos, lyot, fpu),
    )

trait PwfsShapes extends InstrumentShapes:
  import lucuma.core.geom.pwfs.*
  import lucuma.core.enums.GuideProbe

  val posAngle: Angle =
    0.deg

  val offsetPos: Offset =
    Offset.Zero

  val guideStarOffset: Offset =
    Offset(200.arcsec.p, 100.arcsec.q)

  val probe: GuideProbe = GuideProbe.PWFS1

  def shapes: List[ShapeExpression] =
    List(
      ShapeExpression.centeredRectangle(1.arcsec, 1.arcsec).translate(guideStarOffset), // guide star
      patrolField.patrolFieldAt(posAngle, offsetPos),
      probeArm.shapeAt(probe, guideStarOffset, offsetPos)
    )

/**
 * Throwaway demo code to visualize a shape created using `ShapeExpression`s.
 */
class JtsDemo extends Frame("JTS Demo") {
  this: InstrumentShapes =>

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

  def canvas(boundingBox: Boolean) = new Canvas {
    setBackground(Color.lightGray)
    setSize(canvasSize, canvasSize)
    setFocusable(true)
    addKeyListener(new KeyAdapter() {
      override def keyPressed(e: KeyEvent): Unit =
        e.getKeyCode match {
          case KeyEvent.VK_ESCAPE | KeyEvent.VK_Q => System.exit(0)
          case _                                  =>
        }
    })

    override def paint(g: Graphics): Unit = {
      val halfCanvas = canvasSize / 2
      val g2d        = g.asInstanceOf[Graphics2D]
      g2d.setRenderingHints(hints.asJava)
      g2d.setFont(g2d.getFont.deriveFont(8.0f))
      g2d.translate(halfCanvas, halfCanvas)

      // Cross hair showing center.
      g2d.drawLine(-2, 0, 2, 0)
      g2d.drawLine(0, -2, 0, 2)

      // Pixels in each grid square.
      val gpx = Angle.signedDecimalArcseconds.get(gridSize).toDouble / arcsecPerPixel

      val origStroke = g2d.getStroke

      // Use a light dotted line for the grid.
      g2d.setStroke(
        new BasicStroke(0.5f,
                        BasicStroke.CAP_BUTT,
                        BasicStroke.JOIN_MITER,
                        10.0f,
                        Array(1.0f, 3.0f),
                        0.0f
        )
      )

      // Draw the grid.
      (0 to (canvasSize.toDouble / gpx).floor.toInt / 2).foreach { i =>
        // distance from center in pixels
        val dpx = (i * gpx).round.toInt

        // distance from center in arcsec
        val das = (i * Angle.signedDecimalArcseconds.get(gridSize).toDouble).round

        // Draw the labels (p increasing to the left, q increasing upward)
        g2d.drawString(s"$das", -dpx + 5, -halfCanvas + 10)
        if (das != 0L) g2d.drawString(s"-$das", dpx + 5, -halfCanvas + 10)
        g2d.drawString(s"$das", -halfCanvas + 5, -dpx - 5)
        if (das != 0L) g2d.drawString(s"-$das", -halfCanvas + 5, dpx - 5)

        // Draw the grid lines
        g2d.drawLine(-dpx, -halfCanvas, -dpx, halfCanvas)
        g2d.drawLine(dpx, -halfCanvas, dpx, halfCanvas)
        g2d.drawLine(-halfCanvas, -dpx, halfCanvas, -dpx)
        g2d.drawLine(-halfCanvas, dpx, halfCanvas, dpx)
      }

      val originalColor = g2d.getColor
      if (boundingBox) {
        g2d.setStroke(
          new BasicStroke(1f,
                          BasicStroke.CAP_BUTT,
                          BasicStroke.JOIN_MITER,
                          10.0f,
                          Array(2.0f, 4.0f),
                          1.0f
          )
        )

        // Draw the bounding boxes
        shapes.foreach { shapeExpr =>
          shapeExpr.boundingBox.eval match {
            case box: JtsShape =>
              g2d.setPaint(Color.magenta)
              g2d.draw(box.toAwt(arcsecPerPixel))
            case x             => sys.error(s"Whoa unexpected shape type: $x")
          }
        }
      }

      g2d.setPaint(originalColor)
      g2d.setStroke(origStroke)

      // colored shapes first
      coloredShapes.foreach { cs =>
        cs.shape.eval match {
          case jts: JtsShape =>
            g2d.setColor(cs.color)
            cs.stroke.foreach(g2d.setStroke)
            g2d.draw(jts.toAwt(arcsecPerPixel))
            g2d.setStroke(origStroke)
          case x             => sys.error(s"Whoa unexpected shape type: $x")
        }
      }

      // Finally, the basic shapes
      g2d.setColor(Color.BLACK)
      shapes.foreach { shapeExpr =>
        shapeExpr.eval match {
          case jts: JtsShape =>
            g2d.draw(jts.toAwt(arcsecPerPixel))
          case x             => sys.error(s"Whoa unexpected shape type: $x")
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    setSize(canvasSize, canvasSize)

    addWindowListener(new WindowAdapter() {
      override def windowClosing(windowEvent: WindowEvent): Unit =
        System.exit(0)
    })

    add(BorderLayout.CENTER, canvas(args.contains("--boxes")))

    setVisible(true)
  }
}

object JtsGmosLSDemo extends JtsDemo with GmosLSShapes

object JtsFlamingos2LSDemo extends JtsDemo with Flamingos2LSShapes

object JtsGmosImagingDemo extends JtsDemo with GmosImagingShapes

object JtsPwfsDemo extends JtsDemo with PwfsShapes:
  override val arcsecPerPixel: Double = 0.75  // Show full 417" radius patrol field
