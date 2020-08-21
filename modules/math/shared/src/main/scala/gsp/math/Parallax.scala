// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats._
import cats.implicits._
import coulomb._
import coulomb.cats.implicits._
import gsp.math.optics.SplitMono
import gsp.math.units._
import monocle.Iso
import spire.math.Rational

/**
  * Parallax stored as microarcseconds
  * Normally parallax is expressed in milliarcseconds but simbad reports them
  * with a higher precision thus using microarcseconds gives us enough space
  */
final case class Parallax(μas: Quantity[Long, MicroArcSecond]) {
  val mas: Quantity[Rational, MilliArcSecond] = μas.to[Rational, MilliArcSecond]
}

object Parallax extends ParallaxOptics {

  /**
    * The `No parallax`
    * @group Constructors
    */
  val Zero: Parallax = Parallax(0.withUnit[MicroArcSecond])

  /** @group Typeclass Instances */
  implicit val orderParallax: Order[Parallax] =
    Order.by(_.μas)

  /** @group Typeclass Instances */
  implicit val monoidParallax: Monoid[Parallax] =
    Monoid.instance(Zero, (a, b) => Parallax((a.μas.value + b.μas.value).withUnit[MicroArcSecond]))

}

sealed trait ParallaxOptics {

  val μas: Iso[Long, Parallax] =
    Iso[Long, Parallax](v => Parallax(v.withUnit[MicroArcSecond]))(_.μas.value)

  val microarcseconds: Iso[Long, Parallax] = μas

  /**
    * This `Parallax` as signed decimal milliseconds.
    */
  val milliarcseconds: SplitMono[Parallax, BigDecimal] =
    SplitMono
      .fromIso(microarcseconds.reverse)
      .imapB(_.underlying.movePointRight(3).longValue,
             n => new java.math.BigDecimal(n).movePointLeft(3)
      )
}
