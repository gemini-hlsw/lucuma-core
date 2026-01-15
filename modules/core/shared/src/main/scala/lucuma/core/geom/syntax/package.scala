// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.syntax


import algebra.instances.all.*
import coulomb.Quantity
import coulomb.conversion.*
import coulomb.syntax.*
import coulomb.units.accepted.*
import lucuma.core.geom.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.units.*

// Syntax used in the JTS implementation only.
object all extends shapeexpression:
  extension[U](q: Quantity[BigDecimal, U])(using uc: UnitConversion[BigDecimal, U, Millimeter])
    def toTelescopePlane: Quantity[BigDecimal, ArcSecond] =
      uc(q.value).withUnit[Millimeter] * TelescopePlateScale

  extension[U](q: (Quantity[BigDecimal, U], Quantity[BigDecimal, U]))(using UnitConversion[BigDecimal, U, Millimeter])
    def toTelescopePlaneOffset: (Offset.P, Offset.Q) =
      (q._1.toTelescopePlane.toAngle.p, q._2.toTelescopePlane.toAngle.q)

  extension[U](q: Quantity[BigDecimal, U])(using uc: UnitConversion[BigDecimal, U, Millimeter])
    def withPlateScale(ps: Quantity[BigDecimal, ArcSecondPerMillimeter]): Quantity[BigDecimal, ArcSecond] =
      import algebra.instances.all.given
      uc(q.value).withUnit[Millimeter] * ps

    inline def ⨱(ps: Quantity[BigDecimal, ArcSecondPerMillimeter]): Quantity[BigDecimal, ArcSecond] =
      withPlateScale(ps)

  extension[U](q: (Quantity[BigDecimal, U], Quantity[BigDecimal, U]))(using UnitConversion[BigDecimal, U, Millimeter])
    def withPlateScale(ps: Quantity[BigDecimal, ArcSecondPerMillimeter]): (Offset.P, Offset.Q) =
      (q._1.withPlateScale(ps).toAngle.p, q._2.withPlateScale(ps).toAngle.q)

    inline def ⨱(ps: Quantity[BigDecimal, ArcSecondPerMillimeter]): (Offset.P, Offset.Q) =
      withPlateScale(ps)

    def offsetWithPlateScale(ps: Quantity[BigDecimal, ArcSecondPerMillimeter]): Offset =
      Offset(q._1.withPlateScale(ps).toAngle.p, q._2.withPlateScale(ps).toAngle.q)

    inline def ⤇(ps: Quantity[BigDecimal, ArcSecondPerMillimeter]): Offset =
      offsetWithPlateScale(ps)

  extension[U](o: Offset)
    def toDoubleArcseconds: (BigDecimal, BigDecimal) =
      (Angle.signedDecimalArcseconds.get(o.p.toAngle), Angle.signedDecimalArcseconds.get(o.q.toAngle))

