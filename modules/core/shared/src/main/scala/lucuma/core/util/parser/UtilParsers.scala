// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util
package parser

import cats.parse.Parser0
import lucuma.core.parser.MiscParsers.posBigDecimal

trait UtilParsers {

  /** Parses positive time spans in seconds. */
  val posSecondsTimeSpan: Parser0[TimeSpan] =
    posBigDecimal
      .mapFilter(pdb => TimeSpan.fromSeconds(pdb.value))
      .withContext("TimeSpan(seconds)")

}

object UtilParsers extends UtilParsers