// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.parser

import atto._, Atto._
import cats.tests.CatsSuite
import lucuma.core.math.Index
import lucuma.core.math.parser.MiscParsers.index

final class MiscParsersSpec extends CatsSuite {

  test("index parser must be consistent with Index.fromShort") {
    forAll { (s: Short) =>
      index.parseOnly(s.toString).option shouldEqual Index.fromShort.getOption(s)
    }
  }

}
