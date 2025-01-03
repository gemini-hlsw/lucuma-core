// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.syntax


import coulomb.*
import coulomb.conversion.UnitConversion
import coulomb.policy.spire.standard.given
import coulomb.units.accepted.*
import coulomb.syntax.*
import lucuma.core.geom.*
import lucuma.core.math.Offset
import lucuma.core.math.units.*
import spire.std.bigDecimal.*
import lucuma.core.math.Angle

// import scala.language.implicitConversions

// Syntax used in the JTS / JVM implementation only.
object all extends shapeexpression:
  extension[U](q: Quantity[BigDecimal, U])(using UnitConversion[BigDecimal, U, Millimeter])
    def toTelescopePlane: Quantity[BigDecimal, ArcSecond] =
      q.toUnit[Millimeter] * TelescopePlateScale

  extension[U](q: (Quantity[BigDecimal, U], Quantity[BigDecimal, U]))(using UnitConversion[BigDecimal, U, Millimeter])
    def toTelescopePlaneOffset: (Offset.P, Offset.Q) =
      (q._1.toTelescopePlane.toAngle.p, q._2.toTelescopePlane.toAngle.q)

  extension[U](q: Quantity[BigDecimal, U])(using UnitConversion[BigDecimal, U, Millimeter])
    def withPlateScale(ps: Quantity[BigDecimal, ArcSecondPerMillimeter]): Quantity[BigDecimal, ArcSecond] =
      q.toUnit[Millimeter] * ps

  extension[U](q: (Quantity[BigDecimal, U], Quantity[BigDecimal, U]))(using UnitConversion[BigDecimal, U, Millimeter])
    def withPlateScale(ps: Quantity[BigDecimal, ArcSecondPerMillimeter]): (Offset.P, Offset.Q) =
      (q._1.withPlateScale(ps).toAngle.p, q._2.withPlateScale(ps).toAngle.q)

    def withPlateScaleOffset(ps: Quantity[BigDecimal, ArcSecondPerMillimeter]): Offset =
      Offset(q._1.withPlateScale(ps).toAngle.p, q._2.withPlateScale(ps).toAngle.q)

  extension[U](o: Offset)
    def toDoubleArcseconds: (Quantity[BigDecimal, ArcSecond], Quantity[BigDecimal, ArcSecond]) =
      (Angle.signedDecimalArcseconds.get(o.p.toAngle).withUnit[ArcSecond], Angle.signedDecimalArcseconds.get(o.q.toAngle).withUnit[ArcSecond])
