// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.parser

import cats.kernel.laws.discipline.*
import lucuma.core.math.Declination
import lucuma.core.math.arb.ArbAngle
import lucuma.core.math.arb.ArbRightAscension.*
import lucuma.core.optics.laws.discipline.FormatTests
import munit.*

class AngleParsersSuite extends DisciplineSuite with AngleParsers:
  assert(hours.parseAll("0").isRight)
  assert(hours.parseAll("09").isRight)
  assert(hours.parseAll("1").isRight)
  assert(hours.parseAll("01").isRight)
  assert(hours.parseAll("10").isRight)
  assert(hours.parseAll("19").isRight)
  assert(hours.parseAll("22").isRight)
  assert(hours.parseAll("25").isLeft)

  assert(minutes.parseAll("0").isRight)
  assert(minutes.parseAll("09").isRight)
  assert(minutes.parseAll("1").isRight)
  assert(minutes.parseAll("01").isRight)
  assert(minutes.parseAll("10").isRight)
  assert(minutes.parseAll("19").isRight)
  assert(minutes.parseAll("22").isRight)
  assert(minutes.parseAll("59").isRight)
  assert(minutes.parseAll("60").isLeft)

  assert(seconds.parseAll("59").isRight)
  assert(seconds.parseAll("59.").isRight)
  assert(seconds.parseAll("59.1").isRight)
  assert(seconds.parseAll("59.00000000000000").isRight)
  assert(seconds.parseAll("59.1234567890").isRight)

  assert(hms.parseAll("00:00:00.000000").isRight)
  assert(hms.parseAll("01:51:51.10").isRight)
  assert(hms.parseAll("01:51:51.100").isRight)
  assert(hms.parseAll("01:51:51.1000").isRight)
  assert(hms.parseAll("01:51:51.1000100").isRight)
  assert(hms.parseAll("14:29:42.9461331854").isRight)
  assert(hms.parseAll("14:29:42").isRight)

  assert(hms.parseAll("14h 29m 42s").isRight)

  assert(neg.parseAll("-").isRight)
  assert(neg.parseAll("+").isRight)
  assert(neg.parseAll("").isRight)
  assert(dms.parseAll("14:29:42").isRight)
  assert(dms.parseAll("-16:27:46.522175847").isRight)
  assert(dms.parseAll("-16 27 46.522175847").isRight)
  assert(dms.parseAll("+16 27 46.522175847").isRight)
  assert(dms.parseAll("-16° 27′ 46.522175847″").isRight)
  assert(dms.parseAll("-148 00 54.775808").isRight)
  assert(dms.parseAll("-248 00 54.775808").isRight)
  assert(dms.parseAll("+248 00 54.775808").isRight)
  assert(dms.parseAll("+320 00 54.775808").isRight)
