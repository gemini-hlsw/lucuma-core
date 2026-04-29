// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Coordinates
import lucuma.core.math.Region
import lucuma.core.math.arb.ArbAngle.given
import lucuma.core.math.arb.ArbCoordinates.given
import lucuma.core.math.arb.ArbRegion.given
import lucuma.core.model.CloudExtinction
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*
import org.scalacheck.rng.Seed
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle

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

  given Arbitrary[ObservingMode.GhostIfu.type] =
    Arbitrary(Gen.const(ObservingMode.GhostIfu))

  given Cogen[ObservingMode.GhostIfu.type] =
    Cogen.cogenUnit.contramap(_ => ())

  given Arbitrary[ObservingMode.GmosNorthLongSlit] =
    Arbitrary:
      arbitrary[GmosNorthGrating].map(ObservingMode.GmosNorthLongSlit.apply)

  given Cogen[ObservingMode.GmosNorthLongSlit] =
    Cogen[GmosNorthGrating].contramap(_.grating)

  given Arbitrary[ObservingMode.GmosSouthLongSlit] =
    Arbitrary:
      arbitrary[GmosSouthGrating].map(ObservingMode.GmosSouthLongSlit.apply)

  given Cogen[ObservingMode.GmosSouthLongSlit] =
    Cogen[GmosSouthGrating].contramap(_.grating)

  given Arbitrary[ObservingMode.GmosNorthImaging] =
    Arbitrary:
      arbitrary[List[GmosNorthFilter]].map(ObservingMode.GmosNorthImaging.apply)

  given Cogen[ObservingMode.GmosNorthImaging] =
    Cogen[List[GmosNorthFilter]].contramap(_.filters)

  given Arbitrary[ObservingMode.GmosSouthImaging] =
    Arbitrary:
      arbitrary[List[GmosSouthFilter]].map(ObservingMode.GmosSouthImaging.apply)

  given Cogen[ObservingMode.GmosSouthImaging] =
    Cogen[List[GmosSouthFilter]].contramap(_.filters)

  given Arbitrary[ObservingMode.Flamingos2LongSlit] =
    Arbitrary:
      arbitrary[Flamingos2Disperser].map(ObservingMode.Flamingos2LongSlit.apply)

  given Cogen[ObservingMode.Flamingos2LongSlit] =
    Cogen[Flamingos2Disperser].contramap(_.disperser)

  given Arbitrary[ObservingMode.Igrins2LongSlit.type] =
    Arbitrary(Gen.const(ObservingMode.Igrins2LongSlit))

  given Cogen[ObservingMode.Igrins2LongSlit.type] =
    Cogen.cogenUnit.contramap(_ => ())

  given Arbitrary[ObservingMode.Visitor] =
    Arbitrary:
      for 
        m <- arbitrary[VisitorObservingModeType]
        a <- arbitrary[Angle]
      yield ObservingMode.Visitor(m, a)

  given Cogen[ObservingMode.Visitor] =
    Cogen[(VisitorObservingModeType, Angle)].contramap(v => (v.mode, v.radius))

  given Arbitrary[ObservingMode] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[ObservingMode.Flamingos2LongSlit],
        arbitrary[ObservingMode.GhostIfu.type],
        arbitrary[ObservingMode.GmosNorthImaging],
        arbitrary[ObservingMode.GmosNorthLongSlit],
        arbitrary[ObservingMode.GmosSouthImaging],
        arbitrary[ObservingMode.GmosSouthLongSlit],
        arbitrary[ObservingMode.Igrins2LongSlit.type]
      )

  def perturb[A](s: Seed, a: A)(using c: Cogen[A]): Seed =
    c.perturb(s, a)

  given Cogen[ObservingMode] =
    Cogen: (s, m) =>
      m match
        case m: ObservingMode.Flamingos2LongSlit   => perturb(s, m)
        case m: ObservingMode.GhostIfu.type        => perturb(s, m)
        case m: ObservingMode.GmosNorthImaging     => perturb(s, m)
        case m: ObservingMode.GmosNorthLongSlit    => perturb(s, m)
        case m: ObservingMode.GmosSouthImaging     => perturb(s, m)
        case m: ObservingMode.GmosSouthLongSlit    => perturb(s, m)
        case m: ObservingMode.Igrins2LongSlit.type => perturb(s, m)
        case m: ObservingMode.Visitor              => perturb(s, m)

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