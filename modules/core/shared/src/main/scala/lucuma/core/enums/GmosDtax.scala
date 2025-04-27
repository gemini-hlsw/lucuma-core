// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS detector translation X offset.
 * @group Enumerations
 */
enum GmosDtax(
  val tag: String,
  val shortName: String,
  val longName: String,
  val dtax: Int
) derives Enumerated:

  case MinusSix   extends GmosDtax("MinusSix",   "-6", "-6", -6)
  case MinusFive  extends GmosDtax("MinusFive",  "-5", "-5", -5)
  case MinusFour  extends GmosDtax("MinusFour",  "-4", "-4", -4)
  case MinusThree extends GmosDtax("MinusThree", "-3", "-3", -3)
  case MinusTwo   extends GmosDtax("MinusTwo",   "-2", "-2", -2)
  case MinusOne   extends GmosDtax("MinusOne",   "-1", "-1", -1)
  case Zero       extends GmosDtax("Zero",        "0",  "0",  0)
  case One        extends GmosDtax("One",         "1",  "1",  1)
  case Two        extends GmosDtax("Two",         "2",  "2",  2)
  case Three      extends GmosDtax("Three",       "3",  "3",  3)
  case Four       extends GmosDtax("Four",        "4",  "4",  4)
  case Five       extends GmosDtax("Five",        "5",  "5",  5)
  case Six        extends GmosDtax("Six",         "6",  "6",  6)
