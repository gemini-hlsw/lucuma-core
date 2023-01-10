// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.parser

import lucuma.core.math.Index
import lucuma.core.parser.MiscParsers.index
import org.scalacheck.Prop.*

class MiscParsersSuite extends munit.DisciplineSuite {

  test("index parser must be consistent with Index.fromShort") {
    forAll { (s: Short) =>
      assertEquals(index.parseAll(s.toString).toOption,  Index.fromShort.getOption(s))
    }
  }

}
