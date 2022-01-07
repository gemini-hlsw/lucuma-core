// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.parser

import atto._
import lucuma.core.math.Index
import lucuma.core.parser.MiscParsers.index
import org.scalacheck.Prop._

import Atto._

final class MiscParsersSuite extends munit.DisciplineSuite {

  test("index parser must be consistent with Index.fromShort") {
    forAll { (s: Short) =>
      assertEquals(index.parseOnly(s.toString).option,  Index.fromShort.getOption(s))
    }
  }

}
