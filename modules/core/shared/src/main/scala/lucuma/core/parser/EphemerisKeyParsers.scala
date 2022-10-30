// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.parser

import cats.parse.Numbers.digits
import cats.parse.Parser.anyChar
import cats.parse.Parser.string
import cats.parse.*
import cats.syntax.all.*
import lucuma.core.model.EphemerisKey
import lucuma.core.model.EphemerisKey.*

/** Parser for [[lucuma.core.model.EphemerisKey]]. */
trait EphemerisKeyParsers {

  private def des[A](s: String, p: Parser[A]): Parser[A] =
    string(s"${s}_") *> p

  private def textDes[A](s: String)(f: String => A): Parser[A] =
    (string(s"${s}_") *> anyChar.rep0.string.map(f))

  private val signedInt =
    (Parser.char('-').?.with1 ~ digits).string

  private def numDes[A](s: String)(f: Int => A): Parser[A] =
    des(s, signedInt.map(v => f(v.toInt)))

  val comet: Parser[Comet] =
    textDes("Comet")(Comet.apply).withContext("comet")

  val asteroidNew: Parser[AsteroidNew] =
    textDes("AsteroidNew")(AsteroidNew.apply).withContext("asteroidNew")

  val asteroidOld: Parser[AsteroidOld] =
    numDes("AsteroidOld")(AsteroidOld.apply).withContext("asteroidOld")

  val majorBody: Parser[MajorBody] =
    numDes("MajorBody")(MajorBody.apply).withContext("majorBody")

  val userSupplied: Parser[UserSupplied] =
    numDes("UserSupplied")(UserSupplied.apply).withContext("userSupplied")

  val ephemerisKey: Parser[EphemerisKey] =
    (comet.widen[EphemerisKey] |
      asteroidNew.widen[EphemerisKey] |
      asteroidOld.widen[EphemerisKey] |
      majorBody.widen[EphemerisKey] |
      userSupplied.widen[EphemerisKey]).withContext("ephemerisKey")

}

object EphemerisKeyParsers extends EphemerisKeyParsers
