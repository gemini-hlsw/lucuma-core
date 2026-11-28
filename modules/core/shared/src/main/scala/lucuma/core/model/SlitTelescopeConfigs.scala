// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.derived.semiauto
import lucuma.core.enums.OffsetPurpose
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.core.model.sequence.TelescopeConfigSpec
import monocle.Focus
import monocle.Iso
import monocle.Prism
import monocle.macros.GenPrism

enum SlitTelescopeConfigs(val offsetsType: SlitOffsetMode) derives Eq:
  case AlongSlit(value: NonEmptyList[TelescopeConfigAlongSlit]) extends SlitTelescopeConfigs(SlitOffsetMode.NodAlongSlit)
  case ToSky(value: NonEmptyList[TelescopeConfigSpec]) extends SlitTelescopeConfigs(SlitOffsetMode.NodToSky)

  /** The user-specified positions, with along-slit offsets expressed in (p, q). */
  def specs: NonEmptyList[TelescopeConfigSpec] =
    this match
      case AlongSlit(value) => value.map(_.toTelescopeConfigSpec)
      case ToSky(value)     => value

  /**
   * The positions as telescope configs, each purpose resolved from its explicit
   * value or, when unspecified, from `defaultPurpose`.
   */
  def telescopeConfigs(defaultPurpose: Offset => OffsetPurpose): NonEmptyList[TelescopeConfig] =
    specs.map(_.toTelescopeConfig(defaultPurpose))

object SlitTelescopeConfigs:
  val alongSlit: Prism[SlitTelescopeConfigs, SlitTelescopeConfigs.AlongSlit] =
    GenPrism[SlitTelescopeConfigs, SlitTelescopeConfigs.AlongSlit]
  val toSky: Prism[SlitTelescopeConfigs, SlitTelescopeConfigs.ToSky] =
    GenPrism[SlitTelescopeConfigs, SlitTelescopeConfigs.ToSky]

  object AlongSlit:
    val value: Iso[AlongSlit, NonEmptyList[TelescopeConfigAlongSlit]] = Focus[AlongSlit](_.value)
    given Eq[AlongSlit] = semiauto.eq

  object ToSky:
    val value: Iso[ToSky, NonEmptyList[TelescopeConfigSpec]] = Focus[ToSky](_.value)
    given Eq[ToSky] = semiauto.eq
