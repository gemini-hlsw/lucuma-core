// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum GhostBinning(val tag: String, val name: String, val spectralBinning: Int, val spatialBinning: Int) derives Enumerated:
  case OneByOne extends GhostBinning("one_by_one", "1x1", 1, 1)
  case OneByTwo extends GhostBinning("one_by_two", "1x2", 1, 2)
  case OneByFour extends GhostBinning("one_by_four", "1x4", 1, 4)
  case OneByEight extends GhostBinning("one_by_eight", "1x8", 1, 8)
  case TwoByTwo extends GhostBinning("two_by_two", "2x2", 2, 2)
  case TwoByFour extends GhostBinning("two_by_four", "2x4", 2, 4)
  case TwoByEight extends GhostBinning("two_by_eight", "2x8", 2, 8)
  case FourByFour extends GhostBinning("four_by_four", "4x4", 4, 4)

object GhostBinning:
  val Default: GhostBinning = GhostBinning.OneByOne
