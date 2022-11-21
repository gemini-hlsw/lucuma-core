// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums._
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbRefined
import lucuma.core.model.sequence.GmosNodAndShuffle
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbGmosNodAndShuffle {
  import ArbOffset._
  import ArbEnumerated._
  import ArbRefined._

  implicit val arbGmosNodAndShuffle: Arbitrary[GmosNodAndShuffle] = Arbitrary(
    for {
      posA          <- arbitrary[Offset]
      posB          <- arbitrary[Offset]
      eOffset       <- arbitrary[GmosEOffsetting]
      shuffleOffset <- arbitrary[PosInt]
      shuffleCycles <- arbitrary[PosInt]
    } yield GmosNodAndShuffle(posA, posB, eOffset, shuffleOffset, shuffleCycles)
  )

  implicit val cogGmosNodAndShuffle: Cogen[GmosNodAndShuffle] =
    Cogen[(Offset, Offset, GmosEOffsetting, PosInt, PosInt)].contramap(g =>
      (g.posA, g.posB, g.eOffset, g.shuffleOffset, g.shuffleCycles)
    )
}

object ArbGmosNodAndShuffle extends ArbGmosNodAndShuffle
