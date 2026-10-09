// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model.*
import lucuma.core.util.arb.ArbBoundedCollection.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbTelluricType:
  given Arbitrary[TelluricType.ExplicitSpectralTypes] =
    Arbitrary:
      genBoundedNonEmptyList[String](10).map(TelluricType.ExplicitSpectralTypes(_))

  given Arbitrary[TelluricType.UserDefined] =
    Arbitrary:
      Gen.choose(1, 2).map(c => TelluricType.UserDefined(TelluricCount.unsafeFrom(c)))

  given Arbitrary[TelluricType] =
    Arbitrary:
      Gen.oneOf(
        Gen.const(TelluricType.Hot),
        Gen.const(TelluricType.A0V),
        Gen.const(TelluricType.Solar),
        Gen.const(TelluricType.NoTelluric),
        arbitrary[TelluricType.ExplicitSpectralTypes],
        arbitrary[TelluricType.UserDefined]
      )

  given Cogen[TelluricType] =
    Cogen[(String, List[String])].contramap:
      case TelluricType.ExplicitSpectralTypes(types) => ("ExplicitSpectralTypes", types.toList)
      case TelluricType.UserDefined(count)           => ("UserDefined", List(count.value.value.toString))
      case t                                         => (t.tag, Nil)


object ArbTelluricType extends ArbTelluricType
