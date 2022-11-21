// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.parser

import atto._
import cats.syntax.all._
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import lucuma.core.parser.MiscParsers._

import Atto._

/** Parsers for `[[lucuma.core.math.Angle]].` */
trait AngleParsers {

  /**
    * Generic parser for the components of an angle in "11 22 33.444555" format, with an
    * optional decimal point and at most 6 digits following it, and terminal parsers for each segment.
    */
  def genAngle(t1: Parser[_], t2: Parser[_], t3: Parser[_]): Parser[(Int, Int, Int, Int, Int)] =
    (int <~ t1, int <~ t2, int, (char('.') ~> frac(6) | ok(0)) <~ t3)
      .mapN { (h, m, s, µs) =>
        (h, m, s, µs / 1000, µs % 1000)
      }
      .named(s"genAngle($t1, $t2, $t3)")

  /** Generic parser for the components of an HourAngle; see `genAngle`. */
  def genHMS(t1: Parser[_], t2: Parser[_], t3: Parser[_]): Parser[HourAngle] =
    genAngle(t1, t2, t3).map((HourAngle.fromHMS _).tupled).named(s"genHMS($t1, $t2, $t3)")

  /** 00:00:00.000000 */
  val hms1: Parser[HourAngle] =
    genHMS(char(':'), char(':'), void).named("hms1(00:00:00.000000)")

  /** 00 00 00.000000 */
  val hms2: Parser[HourAngle] =
    genHMS(spaces1, spaces1, void).named("hms2(00 00 00.000000)")

  /** 00h 00m 00.000000s */
  val hms3: Parser[HourAngle] =
    genHMS(token(char('h')), token(char('m')), char('s')).named("hms3(00h 00m 00.000000s)")

  val hms: Parser[HourAngle] = (hms1 | hms2 | hms3).named("hms")

  /** Generic parser for the components of a DMS Angle, which is optionally prefixed by a sign. */
  def genDMS(t1: Parser[_], t2: Parser[_], t3: Parser[_]): Parser[Angle] =
    (neg, genAngle(t1, t2, t3).map((Angle.fromDMS _).tupled))
      .mapN {
        case (true, a)  => -a
        case (false, a) => a
      }
      .named(s"genDMS($t2, $t2, $t3)")

  /** +00:00:00.000000 */
  val dms1: Parser[Angle] =
    genDMS(char(':'), char(':'), void).named("dms1(+00:00:00.000000)")

  /** +00 00 00.000000 */
  val dms2: Parser[Angle] =
    genDMS(spaces1, spaces1, void).named("dms2(+00 00 00.000000)")

  /** +04° 41′ 36.2072″ */
  val dms3: Parser[Angle] =
    genDMS(token(char('°')), token(char('′')), char('″')).named("dms3(+04° 41′ 36.2072″)")

  val dms: Parser[Angle] = (dms1 | dms2 | dms3).named("dms")

}

object AngleParsers extends AngleParsers
