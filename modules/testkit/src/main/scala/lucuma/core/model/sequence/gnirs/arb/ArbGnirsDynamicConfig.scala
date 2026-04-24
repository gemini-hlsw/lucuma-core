// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs.arb

import coulomb.Quantity
import coulomb.testkit.given
import eu.timepit.refined.scalacheck.numeric.given
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.*
import lucuma.core.math.arb.ArbRefined.given
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStep
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbNewType.given
import lucuma.core.util.arb.ArbTimeSpan.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbGnirsDynamicConfig:
  import ArbGnirsAcquisitionMirrorMode.given

  given Arbitrary[GnirsFocus] = Arbitrary:
    Gen.oneOf(
      Gen.const(GnirsFocus.Best),
      arbitrary[Quantity[GnirsFocusMotorStepsValue, GnirsFocusMotorStep]].map:
        GnirsFocus.Custom(_)
    )

  given Cogen[GnirsFocus] =
    Cogen[Option[Quantity[GnirsFocusMotorStepsValue, GnirsFocusMotorStep]]]
      .contramap:
        case GnirsFocus.Best          => None
        case GnirsFocus.Custom(value) => Some(value)

  given Arbitrary[GnirsDynamicConfig] = Arbitrary:
    for
      exposure          <- arbitrary[TimeSpan]
      coadds            <- arbitrary[PosInt]
      filter            <- arbitrary[GnirsFilter]
      decker            <- arbitrary[GnirsDecker]
      fpu               <- arbitrary[Either[GnirsFpuSlit, GnirsFpuOther]]
      acquisitionMirror <- arbitrary[GnirsAcquisitionMirrorMode]
      camera            <- arbitrary[GnirsCamera]
      focus             <- arbitrary[GnirsFocus]
      readMode          <- arbitrary[GnirsReadMode]
    yield GnirsDynamicConfig(
      exposure,
      coadds,
      filter,
      decker,
      fpu,
      acquisitionMirror,
      camera,
      focus,
      readMode
    )

  given Cogen[GnirsDynamicConfig] =
    Cogen[
      (
        TimeSpan,
        PosInt,
        GnirsFilter,
        GnirsDecker,
        Either[GnirsFpuSlit, GnirsFpuOther],
        GnirsAcquisitionMirrorMode,
        GnirsCamera,
        GnirsFocus,
        GnirsReadMode
      )
    ].contramap(x =>
      (
        x.exposure,
        x.coadds,
        x.filter,
        x.decker,
        x.fpu,
        x.acquisitionMirror,
        x.camera,
        x.focus,
        x.readMode
      )
    )

object ArbGnirsDynamicConfig extends ArbGnirsDynamicConfig
