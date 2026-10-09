// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.arb

import lucuma.catalog.BlindOffsetCandidate
import lucuma.catalog.BlindOffsetLimits
import lucuma.catalog.CatalogTargetResult
import lucuma.catalog.arb.ArbCatalogTargetResult.given
import lucuma.core.enums.Band
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbAngle.given
import lucuma.core.math.arb.ArbCoordinates.given
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import java.time.Instant

trait ArbBlindOffsetCandidate:
  given Arbitrary[BlindOffsetLimits] =
    Arbitrary:
      for {
        b <- arbitrary[Band]
        s <- Gen.choose(0.0, 15.0)
        f <- Gen.choose(s + 1.0, 25.0)
        o <- Gen.choose(s, f)
      } yield BlindOffsetLimits(
        b,
        BrightnessValue.unsafeFrom(BigDecimal(s)),
        BrightnessValue.unsafeFrom(BigDecimal(f)),
        BrightnessValue.unsafeFrom(BigDecimal(o))
      )

  given Cogen[BlindOffsetLimits] =
    Cogen[(Band, BigDecimal, BigDecimal, BigDecimal)].contramap(l =>
      (l.band, l.bright.value.value, l.faint.value.value, l.optimal.value.value)
    )

  given Arbitrary[BlindOffsetCandidate] =
    Arbitrary:
      for {
        t <- arbitrary[CatalogTargetResult]
        a <- arbitrary[Angle]
        b <- arbitrary[Coordinates]
        c <- arbitrary[Coordinates]
        i <- arbitrary[Instant]
        l <- arbitrary[BlindOffsetLimits]
      } yield BlindOffsetCandidate(t, a, b, c, i, l)

  given Cogen[BlindOffsetCandidate] =
    Cogen[
      (
        CatalogTargetResult,
        Angle,
        Coordinates,
        Coordinates,
        Instant,
        BlindOffsetLimits
      )
    ].contramap(r =>
      (r.catalogResult,
       r.distance,
       r.baseCoordinates,
       r.candidateCoords,
       r.observationTime,
       r.limits
      )
    )

object ArbBlindOffsetCandidate extends ArbBlindOffsetCandidate
