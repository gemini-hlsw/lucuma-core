// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.parser

import cats.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import lucuma.core.parser.MiscParsers.neg
import munit.*

class AngleParsersSuite extends DisciplineSuite with AngleParsers:
  assertEquals(hours.parseAll("0"), 0.asRight)
  assertEquals(hours.parseAll("09"), 9.asRight)
  assertEquals(hours.parseAll("1"), 1.asRight)
  assertEquals(hours.parseAll("01"), 1.asRight)
  assertEquals(hours.parseAll("10"), 10.asRight)
  assertEquals(hours.parseAll("19"), 19.asRight)
  assertEquals(hours.parseAll("22"), 22.asRight)
  assert(hours.parseAll("25").isLeft)

  assertEquals(minutes.parseAll("0"), 0.asRight)
  assertEquals(minutes.parseAll("09"), 9.asRight)
  assertEquals(minutes.parseAll("1"), 1.asRight)
  assertEquals(minutes.parseAll("01"), 1.asRight)
  assertEquals(minutes.parseAll("10"), 10.asRight)
  assertEquals(minutes.parseAll("19"), 19.asRight)
  assertEquals(minutes.parseAll("22"), 22.asRight)
  assertEquals(minutes.parseAll("59"), 59.asRight)
  assert(minutes.parseAll("60").isLeft)

  assertEquals(seconds.parseAll("59"), (59, 0, 0).asRight)
  assertEquals(seconds.parseAll("59."), (59, 0, 0).asRight)
  assertEquals(seconds.parseAll("59.1"), (59, 100, 0).asRight)
  assertEquals(seconds.parseAll("59.00000000000000"), (59, 0, 0).asRight)
  assertEquals(seconds.parseAll("59.1234567890"), (59, 123, 456).asRight)

  assertEquals(hms.parseAll("00:00:00.000000"), HourAngle.angle.reverseGet(Angle.Angle0).asRight)
  assertEquals(hms.parseAll("01:51:51.10"), HourAngle.fromHMS(1, 51, 51, 100, 0).asRight)
  assertEquals(hms.parseAll("01:51:51.1000"), HourAngle.fromHMS(1, 51, 51, 100, 0).asRight)
  assertEquals(hms.parseAll("01:51:51.1000100"), HourAngle.fromHMS(1, 51, 51, 100, 10).asRight)
  assertEquals(hms.parseAll("14:29:42.9461331854"), HourAngle.fromHMS(14, 29, 42, 946, 133).asRight)
  assertEquals(hms.parseAll("14 29 42.9461331854"), HourAngle.fromHMS(14, 29, 42, 946, 133).asRight)
  assertEquals(hms.parseAll("14h 29m 42.9461331854s"), HourAngle.fromHMS(14, 29, 42, 946, 133).asRight)
  assert(neg.parseAll("-").isRight)
  assert(neg.parseAll("+").isRight)
  assert(neg.parseAll("").isRight)

  assertEquals(dms.parseAll("14:29:42"), Angle.fromDMS(14, 29, 42, 0, 0).asRight)
  assertEquals(dms.parseAll("-16:27:46.522175847"), (-Angle.fromDMS(16, 27, 46, 522, 175)).asRight)
  assertEquals(dms.parseAll("+16:27:46.522175847"), Angle.fromDMS(16, 27, 46, 522, 175).asRight)
  assertEquals(dms.parseAll("16° 27′ 46.522175847″"), Angle.fromDMS(16, 27, 46, 522, 175).asRight)
  assertEquals(dms.parseAll("-148 00 54.775808"), (-Angle.fromDMS(148, 0, 54, 775, 808)).asRight)
  assertEquals(dms.parseAll("-248 00 54.775808"), (-Angle.fromDMS(248, 0, 54, 775, 808)).asRight)
  assertEquals(dms.parseAll("+320 00 54.775808"), Angle.fromDMS(320, 0, 54, 775, 808).asRight)

  assert(dms.parseAll("+360 00 54.775808").isLeft)
