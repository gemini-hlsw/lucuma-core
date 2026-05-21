// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.core.util.arb.ArbBoundedCollection
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbSlitTelescopeConfigs:
  import ArbBoundedCollection.*
  import ArbTelescopeConfig.given
  import ArbTelescopeConfigAlongSlit.given

  given Arbitrary[SlitTelescopeConfigs] =
    Arbitrary:
      arbitrary[Boolean].flatMap:
        case true  =>
          genBoundedNonEmptyList[TelescopeConfigAlongSlit](BoundedCollectionLimit)
            .map(SlitTelescopeConfigs.AlongSlit(_))
        case false =>
          genBoundedNonEmptyList[TelescopeConfig](BoundedCollectionLimit)
            .map(SlitTelescopeConfigs.ToSky(_))

  given Cogen[SlitTelescopeConfigs] =
    Cogen[(Option[List[TelescopeConfigAlongSlit]], Option[List[TelescopeConfig]])].contramap:
      case SlitTelescopeConfigs.AlongSlit(vs) => (Some(vs.toList), None)
      case SlitTelescopeConfigs.ToSky(vs)     => (None, Some(vs.toList))

object ArbSlitTelescopeConfigs extends ArbSlitTelescopeConfigs
