// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.parser

import cats.parse.Parser.char
import cats.parse.Parser.charIn
import cats.parse.Parser.string
import cats.parse.Rfc5234.*
import cats.parse.*
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import lucuma.core.optics.Format

trait AngleParsers:
  val colon        = char(':')
  val colonOrSpace = (char(':') | sp).void
  val neg = char('-').map(_ => true).backtrack | char('+').?.map(_ => false)

  val hours = (char('1') ~ digit).backtrack // 10-19
    .orElse(char('2') ~ charIn('0' to '4')) // 20-24
    .backtrack
    .orElse(char('0').? *> digit)           // 00 - 09
    .backtrack
    .orElse(char('0'))                      // plain 0
    .string
    .map(_.toInt)
    .withContext("hours")

  val minutes = (charIn('0' to '5') ~ digit).backtrack // 10-19
    .orElse(char('5') ~ digit)          // 20-24
    .backtrack
    .orElse(char('0').?.with1 *> digit) // 00 - 09
    .backtrack
    .orElse(char('0'))                  // plain 0
    .string
    .map(_.toInt)
    .withContext("minutes")

  val seconds =
    // allow any amount of decimals but only use 6
    (minutes ~ char('.').? ~ digit.rep(1, 3).?.string ~ digit.rep(1, 3).?.string ~ digit.rep.?)
      .map { case ((((s, _), d1), d2), _) =>
        (s,
         if (d1.isEmpty) BigDecimal(0) else BigDecimal(d1.padTo(3, '0')),
         if (d2.isEmpty) BigDecimal(0) else BigDecimal(d2)
        )
      }
    .withContext("seconds")

  private val hmsParser1 =
    (hours ~ colonOrSpace.void ~ minutes ~ colonOrSpace.void ~ seconds).map {
      case ((((h, _), m), _), (s, ms, µs)) =>
        HourAngle.fromHMS(h, m, s, ms.toInt, µs.toInt)
    }

  private val hmsParser2 =
    (hours ~ (char('h') ~ sp.?).void ~
      minutes ~ (char('m') ~ sp.?).void ~
      seconds ~ char('s'))
      .map { case (((((h, _), m), _), (s, ms, µs)), _) =>
        HourAngle.fromHMS(h, m, s, ms.toInt, µs.toInt)
      }

  val hms = (hmsParser1.backtrack | hmsParser2)
    .withContext("hms")

  val degrees =
    (char('3') ~ charIn('0' to '5') ~ digit) // 300-359
    .backtrack
    .orElse(charIn('1' to '2') ~ digit ~ digit) // 100-299
    .backtrack
    .orElse(charIn('0' to '9') ~ digit) // 10-99
    .backtrack
    .orElse(char('0').? *> digit) // 00 - 09
    .backtrack
    .orElse(char('0'))            // plain 0
    .string
    .map(_.toInt)

  private val dmsParser1 =
    (neg ~ degrees ~ colonOrSpace ~ minutes ~ colonOrSpace ~ seconds).map {
      case (((((neg, h), _), m), _), (s, ms, µs)) =>
        val r = Angle.fromDMS(h, m, s, ms.toInt, µs.toInt)
        if (neg) -r else r
    }

  private val dmsParser2 =
    (neg ~ degrees ~ (char('°') ~ sp.?).void ~
      minutes ~ (char('′') ~ sp.?).void ~
      seconds ~ char('″').void).map { case ((((((neg, h), _), m), _), (s, ms, µs)), _) =>
      val r = Angle.fromDMS(h, m, s, ms.toInt, µs.toInt)
      if (neg) -r else r
    }

  val dms = (dmsParser1.backtrack | dmsParser2)
    .withContext("dms")

object AngleParsers extends AngleParsers
