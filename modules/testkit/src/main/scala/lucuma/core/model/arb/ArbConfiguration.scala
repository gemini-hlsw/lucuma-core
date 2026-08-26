// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosNorthIfuFpu
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosSouthIfuFpu
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Angle
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

  given Arbitrary[ObservingMode.GmosNorthMos] =
    Arbitrary:
      arbitrary[GmosNorthGrating].map(ObservingMode.GmosNorthMos.apply)

  given Cogen[ObservingMode.GmosNorthMos] =
    Cogen[GmosNorthGrating].contramap(_.grating)

  given Arbitrary[ObservingMode.GmosNorthIfu] =
    Arbitrary:
      for
        g <- arbitrary[GmosNorthGrating]
        u <- arbitrary[GmosNorthIfuFpu]
      yield ObservingMode.GmosNorthIfu(g, u)

  given Cogen[ObservingMode.GmosNorthIfu] =
    Cogen[(GmosNorthGrating, GmosNorthIfuFpu)].contramap(m => (m.grating, m.fpu))

  given Arbitrary[ObservingMode.GmosSouthIfu] =
    Arbitrary:
      for
        g <- arbitrary[GmosSouthGrating]
        u <- arbitrary[GmosSouthIfuFpu]
      yield ObservingMode.GmosSouthIfu(g, u)

  given Cogen[ObservingMode.GmosSouthIfu] =
    Cogen[(GmosSouthGrating, GmosSouthIfuFpu)].contramap(m => (m.grating, m.fpu))

  given Arbitrary[ObservingMode.GmosSouthLongSlit] =
    Arbitrary:
      arbitrary[GmosSouthGrating].map(ObservingMode.GmosSouthLongSlit.apply)

  given Cogen[ObservingMode.GmosSouthLongSlit] =
    Cogen[GmosSouthGrating].contramap(_.grating)

  given Arbitrary[ObservingMode.GmosSouthMos] =
    Arbitrary:
      arbitrary[GmosSouthGrating].map(ObservingMode.GmosSouthMos.apply)

  given Cogen[ObservingMode.GmosSouthMos] =
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

  given Arbitrary[ObservingMode.Flamingos2Mos] =
    Arbitrary:
      arbitrary[Flamingos2Disperser].map(ObservingMode.Flamingos2Mos.apply)

  given Cogen[ObservingMode.Flamingos2Mos] =
    Cogen[Flamingos2Disperser].contramap(_.disperser)

  given Arbitrary[ObservingMode.Igrins2LongSlit.type] =
    Arbitrary(Gen.const(ObservingMode.Igrins2LongSlit))

  given Cogen[ObservingMode.Igrins2LongSlit.type] =
    Cogen.cogenUnit.contramap(_ => ())

  given Arbitrary[ObservingMode.GnirsLongSlit] =
    Arbitrary:
      for
        g <- arbitrary[GnirsGrating]
        c <- arbitrary[GnirsCamera]
        p <- arbitrary[GnirsPrism]
      yield ObservingMode.GnirsLongSlit(g, c, p)

  given Cogen[ObservingMode.GnirsLongSlit] =
    Cogen[(GnirsGrating, GnirsCamera, GnirsPrism)].contramap(m => (m.grating, m.camera, m.prism))

  given Arbitrary[ObservingMode.GnirsIfu] =
    Arbitrary:
      for
        g <- arbitrary[GnirsGrating]
        f <- arbitrary[GnirsFpuIfu]
      yield ObservingMode.GnirsIfu(g, f)

  given Cogen[ObservingMode.GnirsIfu] =
    Cogen[(GnirsGrating, GnirsFpuIfu)].contramap(m => (m.grating, m.fpu))

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
        arbitrary[ObservingMode.Flamingos2Mos],
        arbitrary[ObservingMode.GhostIfu.type],
        arbitrary[ObservingMode.GmosNorthImaging],
        arbitrary[ObservingMode.GmosNorthLongSlit],
        arbitrary[ObservingMode.GmosNorthMos],
        arbitrary[ObservingMode.GmosSouthImaging],
        arbitrary[ObservingMode.GmosSouthLongSlit],
        arbitrary[ObservingMode.GmosSouthMos],
        arbitrary[ObservingMode.GmosNorthIfu],
        arbitrary[ObservingMode.GmosSouthIfu],
        arbitrary[ObservingMode.Igrins2LongSlit.type],
        arbitrary[ObservingMode.GnirsLongSlit],
        arbitrary[ObservingMode.GnirsIfu],
        arbitrary[ObservingMode.Visitor]
      )

  def perturb[A](s: Seed, a: A)(using c: Cogen[A]): Seed =
    c.perturb(s, a)

  given Cogen[ObservingMode] =
    Cogen: (s, m) =>
      m match
        case m: ObservingMode.Flamingos2LongSlit   => perturb(s, m)
        case m: ObservingMode.Flamingos2Mos        => perturb(s, m)
        case m: ObservingMode.GhostIfu.type        => perturb(s, m)
        case m: ObservingMode.GmosNorthImaging     => perturb(s, m)
        case m: ObservingMode.GmosNorthLongSlit    => perturb(s, m)
        case m: ObservingMode.GmosNorthMos         => perturb(s, m)
        case m: ObservingMode.GmosSouthImaging     => perturb(s, m)
        case m: ObservingMode.GmosSouthLongSlit    => perturb(s, m)
        case m: ObservingMode.GmosSouthMos         => perturb(s, m)
        case m: ObservingMode.GmosNorthIfu         => perturb(s, m)
        case m: ObservingMode.GmosSouthIfu         => perturb(s, m)
        case m: ObservingMode.Igrins2LongSlit.type => perturb(s, m)
        case m: ObservingMode.GnirsLongSlit        => perturb(s, m)
        case m: ObservingMode.GnirsIfu             => perturb(s, m)
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