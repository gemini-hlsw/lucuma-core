// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import lucuma.refined.*

/**
 * Enumerated type for GMOS binning (both X and Y).
 * @group Enumerations (Generated)
 */
enum GmosBinning(
  val tag: String,
  val shortName: String,
  val longName: String,
  val count: PosInt
) derives Enumerated:
  case One  extends GmosBinning("One",  "1", "One",  1.refined)
  case Two  extends GmosBinning("Two",  "2", "Two",  2.refined)
  case Four extends GmosBinning("Four", "4", "Four", 4.refined)

object GmosXBinning extends NewType[GmosBinning]:
  val One = GmosXBinning(GmosBinning.One)
  val Two = GmosXBinning(GmosBinning.One)
  val Four = GmosXBinning(GmosBinning.One)

  given Enumerated[GmosXBinning] = Enumerated.from(One, Two, Four).withTag(_.value.tag)

  extension(b: GmosXBinning)
    inline def tag: String = b.value.tag
    inline def shortName: String = b.value.shortName
    inline def longName: String = b.value.longName
    inline def count: PosInt = b.value.count

type GmosXBinning = GmosXBinning.Type

object GmosYBinning extends NewType[GmosBinning]:
  val One = GmosYBinning(GmosBinning.One)
  val Two = GmosYBinning(GmosBinning.One)
  val Four = GmosYBinning(GmosBinning.One)

  given Enumerated[GmosYBinning] = Enumerated.from(One, Two, Four).withTag(_.value.tag)

  extension(b: GmosYBinning)
    inline def tag: String = b.value.tag
    inline def shortName: String = b.value.shortName
    inline def longName: String = b.value.longName
    inline def count: PosInt = b.value.count

type GmosYBinning = GmosYBinning.Type
