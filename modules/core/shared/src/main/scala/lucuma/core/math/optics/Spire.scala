// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.optics

import spire.math._
import scala.util.Try
import monocle.Iso
import gsp.math.optics.Format
import gsp.math.optics.SplitEpi

object Spire {
  val numberInt: SplitEpi[Number, Int] = SplitEpi(_.intValue, Number.apply)

  val numberLong: SplitEpi[Number, Long] = SplitEpi(_.longValue, Number.apply)

  // Deal with Infinity and overflows
  val numberFloat: SplitEpi[Option[Number], Option[Float]] =
    SplitEpi(
      _.map(_.floatValue).filterNot(f => java.lang.Float.isNaN(f) || java.lang.Float.isInfinite(f)),
      _.flatMap(f => Try(Number(f)).toOption)
    )

  // Deal with Infinity and overflows
  val numberDouble: SplitEpi[Option[Number], Option[Double]] =
    SplitEpi(
      _.map(_.doubleValue).filterNot(f =>
        java.lang.Double.isNaN(f) || java.lang.Double.isInfinite(f)
      ),
      _.flatMap(f => Try(Number(f)).toOption)
    )

  val numberBigInt: SplitEpi[Number, BigInt] = SplitEpi(_.toBigInt, Number.apply)

  val numberSafeLong: SplitEpi[Number, SafeLong] = SplitEpi(n => SafeLong(n.toBigInt), Number.apply)

  val numberBigDecimal: Iso[Number, BigDecimal] = Iso((n: Number) => n.toBigDecimal)(Number.apply)

  // Number <-> Rational is actually an Iso in the JVM and a SplitEpi in JS, because of Long precision.
  // val numberRational: SplitEpi[Number, Rational] = SplitEpi(_.toRational, Number.apply)
  // val numberRational: Iso[Number, Rational]      = Iso((n: Number) => n.toRational)(Number.apply)

  val numberNatural: Format[Number, Natural] =
    Format(n => Try(Natural(n.toBigInt)).toOption, Number.apply)
}
