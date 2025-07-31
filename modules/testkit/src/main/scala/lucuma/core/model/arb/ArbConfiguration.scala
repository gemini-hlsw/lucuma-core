// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Coordinates
import lucuma.core.math.Region
import lucuma.core.math.arb.ArbCoordinates.given
import lucuma.core.math.arb.ArbRegion.given
import lucuma.core.model.CloudExtinction
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbConfiguration:
  import Configuration.Conditions
  import Configuration.ObservingMode

  given Arbitrary[Conditions] =
    Arbitrary:
      for
        ce <- arbitrary[CloudExtinction.Preset]
        iq <- arbitrary[ImageQuality.Preset]
        sb <- arbitrary[SkyBackground]
        wv <- arbitrary[WaterVapor]
      yield Conditions(ce, iq, sb, wv)
    

  given Cogen[Conditions] =
    Cogen[(CloudExtinction.Preset, ImageQuality.Preset, SkyBackground, WaterVapor)]
      .contramap(c => (c.cloudExtinction,c.imageQuality,  c.skyBackground, c.waterVapor))

  given Arbitrary[ObservingMode.GmosNorthLongSlit] =
    Arbitrary:
      arbitrary[GmosNorthGrating].map(ObservingMode.GmosNorthLongSlit(_))

  given Cogen[ObservingMode.GmosNorthLongSlit] =
    Cogen[GmosNorthGrating].contramap(_.grating)

  given Arbitrary[ObservingMode.GmosSouthLongSlit] =
    Arbitrary:
      arbitrary[GmosSouthGrating].map(ObservingMode.GmosSouthLongSlit(_))

  given Cogen[ObservingMode.GmosSouthLongSlit] =
    Cogen[GmosSouthGrating].contramap(_.grating)

  given Arbitrary[ObservingMode] =
    Arbitrary:
      Gen.oneOf(arbitrary[ObservingMode.GmosNorthLongSlit], arbitrary[Configuration.ObservingMode.GmosSouthLongSlit])

  given Cogen[ObservingMode] =
    Cogen[Either[ObservingMode.GmosNorthLongSlit, ObservingMode.GmosSouthLongSlit]].contramap { 
      case n @ ObservingMode.GmosNorthLongSlit(_) => Left(n)
      case s @ ObservingMode.GmosSouthLongSlit(_) => Right(s)
    }

  given Arbitrary[Configuration] =
    Arbitrary:
      for
        c <- arbitrary[Conditions]
        r <- arbitrary[Either[Coordinates, Region]]
        o <- arbitrary[ObservingMode]
      yield (Configuration(c, r, o))
  
  given Cogen[Configuration] =
    Cogen[(Conditions, Either[Coordinates, Region], ObservingMode)]
      .contramap(c => (c.conditions, c.target, c.observingMode))

object ArbConfiguration extends ArbConfiguration
