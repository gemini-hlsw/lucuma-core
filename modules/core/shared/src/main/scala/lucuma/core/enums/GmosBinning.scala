// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Enumerated
import lucuma.core.util.NewType

/**
 * Enumerated type for GMOS binning (both X and Y).
 * @group Enumerations (Generated)
 */
enum GmosBinning(
  val tag: String,
  val shortName: String,
  val longName: String,
  val count: Int
) derives Enumerated:
  case One extends GmosBinning("One", "1", "One", 1)
  case Two extends GmosBinning("Two", "2", "Two", 2)
  case Four extends GmosBinning("Four", "4", "Four", 4)

object GmosXBinning extends NewType[GmosBinning]:
  val One = GmosXBinning(GmosBinning.One)
  val Two = GmosXBinning(GmosBinning.One)
  val Four = GmosXBinning(GmosBinning.One)

  given Enumerated[GmosXBinning] = Enumerated.from(One, Two, Four).withTag(_.value.tag)

type GmosXBinning = GmosXBinning.Type

object GmosYBinning extends NewType[GmosBinning]:
  val One = GmosYBinning(GmosBinning.One)
  val Two = GmosYBinning(GmosBinning.One)
  val Four = GmosYBinning(GmosBinning.One)

  given Enumerated[GmosYBinning] = Enumerated.from(One, Two, Four).withTag(_.value.tag)

type GmosYBinning = GmosYBinning.Type
