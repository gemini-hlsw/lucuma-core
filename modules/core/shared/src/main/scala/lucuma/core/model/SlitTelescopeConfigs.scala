// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.derived.semiauto
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import monocle.Focus
import monocle.Iso
import monocle.Prism
import monocle.macros.GenPrism

enum SlitTelescopeConfigs(val offsetsType: SlitOffsetMode) derives Eq:
  case AlongSlit(value: NonEmptyList[TelescopeConfigAlongSlit]) extends SlitTelescopeConfigs(SlitOffsetMode.NodAlongSlit)
  case ToSky(value: NonEmptyList[TelescopeConfig]) extends SlitTelescopeConfigs(SlitOffsetMode.NodToSky)

  def telescopeConfigs: NonEmptyList[TelescopeConfig] =
    this match
      case AlongSlit(value) => value.map(_.toTelescopeConfig)
      case ToSky(telescopeConfigs) => telescopeConfigs

object SlitTelescopeConfigs:
  val alongSlit: Prism[SlitTelescopeConfigs, SlitTelescopeConfigs.AlongSlit] =
    GenPrism[SlitTelescopeConfigs, SlitTelescopeConfigs.AlongSlit]
  val toSky: Prism[SlitTelescopeConfigs, SlitTelescopeConfigs.ToSky] =
    GenPrism[SlitTelescopeConfigs, SlitTelescopeConfigs.ToSky]

  object AlongSlit:
    val value: Iso[AlongSlit, NonEmptyList[TelescopeConfigAlongSlit]] = Focus[AlongSlit](_.value)
    given Eq[AlongSlit] = semiauto.eq

  object ToSky:
    val value: Iso[ToSky, NonEmptyList[TelescopeConfig]] = Focus[ToSky](_.value)
    given Eq[ToSky] = semiauto.eq
