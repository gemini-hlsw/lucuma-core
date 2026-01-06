// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.arb

import lucuma.catalog.BlindOffsetCandidate
import lucuma.catalog.CatalogTargetResult
import lucuma.catalog.arb.ArbCatalogTargetResult.given
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbAngle.given
import lucuma.core.math.arb.ArbCoordinates.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

import java.time.Instant

trait ArbBlindOffsetCandidate:
  given Arbitrary[BlindOffsetCandidate] =
    Arbitrary:
      for {
        t <- arbitrary[CatalogTargetResult]
        a <- arbitrary[Angle]
        b <- arbitrary[Coordinates]
        c <- arbitrary[Coordinates]
        i <- arbitrary[Instant]
      } yield BlindOffsetCandidate(t, a, b, c, i)

  given Cogen[BlindOffsetCandidate] =
    Cogen[
      (
        CatalogTargetResult,
        Angle,
        Coordinates,
        Coordinates,
        Instant
      )
    ].contramap(r =>
      (r.catalogResult, r.distance, r.baseCoordinates, r.candidateCoords, r.observationTime)
    )

object ArbBlindOffsetCandidate extends ArbBlindOffsetCandidate
