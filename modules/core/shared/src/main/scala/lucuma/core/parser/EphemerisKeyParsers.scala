// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package parser

import cats.parse.*
import cats.parse.Numbers.digits
import cats.parse.Parser.anyChar
import cats.parse.Parser.string
import cats.syntax.all.*
import lucuma.core.model.Ephemeris.Key.*

/** Parser for [[lucuma.core.model.Ephemeris.Key]]. */
trait EphemerisKeyParsers {

  private def des[A](s: String, p: Parser[A]): Parser[A] =
    string(s"${s}_") *> p

  private def textDes[A](s: String)(f: String => A): Parser[A] =
    (string(s"${s}_") *> anyChar.rep0.string.map(f))

  private val signedInt =
    (Parser.char('-').?.with1 ~ digits).string

  private def numDes[A](s: String)(f: Int => A): Parser[A] =
    des(s, signedInt.map(v => f(v.toInt)))

  private def longDes[A](s: String)(f: Long => A): Parser[A] =
    des(s, signedInt.map(v => f(v.toLong)))

  val comet: Parser[Comet] =
    textDes("Comet")(Comet.apply).withContext("comet")

  val asteroidNew: Parser[AsteroidNew] =
    textDes("AsteroidNew")(AsteroidNew.apply).withContext("asteroidNew")

  val asteroidOld: Parser[AsteroidOld] =
    numDes("AsteroidOld")(AsteroidOld.apply).withContext("asteroidOld")

  val majorBody: Parser[MajorBody] =
    numDes("MajorBody")(MajorBody.apply).withContext("majorBody")

  val userSupplied: Parser[UserSupplied] =
    longDes("UserSupplied")(UserSupplied.apply).withContext("userSupplied")

  val ephemerisKey: Parser[Ephemeris.Key] =
    (comet.widen[Ephemeris.Key] |
      asteroidNew.widen[Ephemeris.Key] |
      asteroidOld.widen[Ephemeris.Key] |
      majorBody.widen[Ephemeris.Key] |
      userSupplied.widen[Ephemeris.Key]).withContext("ephemerisKey")

}

object EphemerisKeyParsers extends EphemerisKeyParsers
