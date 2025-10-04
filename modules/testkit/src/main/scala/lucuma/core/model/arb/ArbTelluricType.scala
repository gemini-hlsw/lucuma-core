// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model.*
import lucuma.core.util.arb.ArbBoundedCollection.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbTelluricType:
  given Arbitrary[TelluricType.Manual] =
    Arbitrary:
      genBoundedNonEmptyList[String](10).map(TelluricType.Manual(_))

  given Arbitrary[TelluricType] =
    Arbitrary:
      Gen.oneOf(
        Gen.const(TelluricType.Hot),
        Gen.const(TelluricType.A0V),
        Gen.const(TelluricType.Solar),
        arbitrary[TelluricType.Manual]
      )

  given Cogen[TelluricType] =
    Cogen[Either[Unit, Either[Unit, Either[Unit, List[String]]]]].contramap:
      case TelluricType.Hot           => Left(())
      case TelluricType.A0V           => Right(Left(()))
      case TelluricType.Solar         => Right(Right(Left(())))
      case TelluricType.Manual(types) => Right(Right(Right(types.toList)))


object ArbTelluricType extends ArbTelluricType
