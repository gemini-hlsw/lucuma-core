// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

/**
 * Possible telluric calibration type
 */
sealed trait TelluricType(val tag: String) derives Eq

object TelluricType:
  case object Hot                                     extends TelluricType("Hot")
  case object A0V                                     extends TelluricType("A0V")
  case object Solar                                   extends TelluricType("Solar")
  case class  Manual(starTypes: NonEmptyList[String]) extends TelluricType("Manual") derives Eq

  object Manual:
    val starTypes: Lens[Manual, NonEmptyList[String]] = Focus[Manual](_.starTypes)

  val hot: Prism[TelluricType, TelluricType.Hot.type] =
    GenPrism[TelluricType, TelluricType.Hot.type]

  val a0v: Prism[TelluricType, TelluricType.A0V.type] =
    GenPrism[TelluricType, TelluricType.A0V.type]

  val solar: Prism[TelluricType, TelluricType.Solar.type] =
    GenPrism[TelluricType, TelluricType.Solar.type]

  val manual: Prism[TelluricType, TelluricType.Manual] =
    GenPrism[TelluricType, TelluricType.Manual]
