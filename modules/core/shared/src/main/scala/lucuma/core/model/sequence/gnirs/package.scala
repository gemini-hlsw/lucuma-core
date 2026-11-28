// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs

import cats.data.NonEmptyList
import cats.syntax.order.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsSlitOffsetPreset
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.core.model.sequence.TelescopeConfigSpec

// ATTENTION: This logic is duplicated in the DB view in the ODB. Modify it there too if it's changed here.
// The actual requirements actually use the wavelength instead of the filter, but the default wavelength
// is the filter's optimalWavelength, so we shortcircuit and use the filter directly.
def alongSlitDefaultTelescopeConfigs(prism: GnirsPrism, camera: GnirsCamera, wavelength: GnirsGratingWavelength): NonEmptyList[TelescopeConfigAlongSlit] =
  val LongCameraCutoff: Wavelength = Wavelength.fromIntNanometers(2500).get
  val arcSecs: NonEmptyList[BigDecimal] =
    if prism === GnirsPrism.Sxd || prism === GnirsPrism.Lxd then
      NonEmptyList.of(-1, 2, 2, -1)
    else if camera === GnirsCamera.ShortBlue || camera === GnirsCamera.ShortRed then
      NonEmptyList.of(2, -4, -4, 2)
    else if wavelength.value >= LongCameraCutoff then
      NonEmptyList.of(-3, 3, 3, -3)
    else
      NonEmptyList.of(-1, 5, 5, -1)
  arcSecs.map(arcSec => TelescopeConfigAlongSlit(arcSec.qArcsec, StepGuideState.Enabled))

val OnSkyDefaultTelescopeConfigs: NonEmptyList[TelescopeConfigSpec] =
  NonEmptyList.of(
    TelescopeConfigSpec.objectAt(Offset.Zero),
    TelescopeConfigSpec.skyAt(Offset(30.pArcsec, 0.qArcsec)),
    TelescopeConfigSpec.skyAt(Offset(30.pArcsec, 0.qArcsec)),
    TelescopeConfigSpec.objectAt(Offset.Zero)
  )

def defaultSlitTelescopeConfigs(preset: GnirsSlitOffsetPreset, prism: GnirsPrism, camera: GnirsCamera, wavelength: GnirsGratingWavelength): SlitTelescopeConfigs =
  preset match
    case GnirsSlitOffsetPreset.NodAlongSlit => SlitTelescopeConfigs.AlongSlit(alongSlitDefaultTelescopeConfigs(prism, camera, wavelength))
    case GnirsSlitOffsetPreset.NodToSky     => SlitTelescopeConfigs.ToSky(OnSkyDefaultTelescopeConfigs)

// GNIRS IFU telescope-config presets. Unlike the long-slit defaults these are not
// derived from the optical config; they are fixed templates.
// The head of each list is the default value, used to initialize observing modes at creation.
private val LowResolutionIfuPresets: NonEmptyList[(String, NonEmptyList[TelescopeConfigSpec])] =
  NonEmptyList.of(
    "Extended" -> NonEmptyList.of(
      TelescopeConfigSpec.objectAt(Offset(  0.15.pArcsec,    0.15.qArcsec)),
      TelescopeConfigSpec.skyAt(Offset(    10.pArcsec,      10.qArcsec)),
      TelescopeConfigSpec.objectAt(Offset(-(0.15.pArcsec), -(0.15.qArcsec))),
      TelescopeConfigSpec.skyAt(Offset(  -(10.pArcsec),   -(10.qArcsec)))
    ),
    "Point"    -> NonEmptyList.of(
      TelescopeConfigSpec.objectAt(Offset(  0.75.pArcsec,    1.5.qArcsec)),
      TelescopeConfigSpec.objectAt(Offset(-(0.75.pArcsec), -(1.5.qArcsec))),
      TelescopeConfigSpec.objectAt(Offset(-(0.75.pArcsec),   1.5.qArcsec)),
      TelescopeConfigSpec.objectAt(Offset(  0.75.pArcsec,  -(1.5.qArcsec)))
    )
  )

private val HighResolutionIfuPresets: NonEmptyList[(String, NonEmptyList[TelescopeConfigSpec])] =
  NonEmptyList.of(
    "Science"  -> NonEmptyList.of(
      TelescopeConfigSpec.objectAt(Offset(  0.1.pArcsec, -(0.1.qArcsec))),
      TelescopeConfigSpec.skyAt(Offset(   10.pArcsec,    10.qArcsec)),
      TelescopeConfigSpec.objectAt(Offset(-(0.1.pArcsec),  0.1.qArcsec)),
      TelescopeConfigSpec.skyAt(Offset( -(10.pArcsec), -(10.qArcsec)))
    ),
    "Telluric" -> NonEmptyList.of(
      TelescopeConfigSpec.objectAt(Offset(  0.pArcsec,    0.qArcsec)),
      TelescopeConfigSpec.skyAt(Offset(  5.pArcsec,    5.qArcsec)),
      TelescopeConfigSpec.objectAt(Offset(  0.pArcsec,    0.qArcsec)),
      TelescopeConfigSpec.objectAt(Offset(  0.pArcsec,    0.qArcsec)),
      TelescopeConfigSpec.skyAt(Offset(-(5.pArcsec), -(5.qArcsec))),
      TelescopeConfigSpec.objectAt(Offset(  0.pArcsec,    0.qArcsec))
    )
  )

def gnirsIfuTelescopeConfigPresets(fpu: GnirsFpuIfu): NonEmptyList[(String, NonEmptyList[TelescopeConfigSpec])] =
  fpu match
    case GnirsFpuIfu.LowResolution  => LowResolutionIfuPresets
    case GnirsFpuIfu.HighResolution => HighResolutionIfuPresets

// The default IFU telescope configs seeded at observing-mode creation (the first preset).
def defaultIfuTelescopeConfigs(fpu: GnirsFpuIfu): NonEmptyList[TelescopeConfigSpec] =
  gnirsIfuTelescopeConfigPresets(fpu).head._2
