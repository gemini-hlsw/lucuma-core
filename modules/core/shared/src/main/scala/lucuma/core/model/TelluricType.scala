// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.numeric.Interval
import lucuma.core.util.NewBoolean
import lucuma.core.util.NewRefined
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

/** How many telluric observations the user supplies: one after the science, or one before and one after. */
object TelluricCount extends NewRefined[Int, Interval.Closed[1, 2]]
type TelluricCount = TelluricCount.Type

/** Whether a telluric calibration's target and configuration are the user's rather than generated. */
object IsUserDefinedTelluric extends NewBoolean
type IsUserDefinedTelluric = IsUserDefinedTelluric.Type

/**
 * Possible telluric calibration type
 */
sealed trait TelluricType(val tag: String) derives Eq

object TelluricType:
  case object Hot                                                    extends TelluricType("Hot")
  case object A0V                                                    extends TelluricType("A0V")
  case object Solar                                                  extends TelluricType("Solar")
  case class  ExplicitSpectralTypes(starTypes: NonEmptyList[String]) extends TelluricType("ExplicitSpectralTypes") derives Eq
  case class  UserDefined(count: TelluricCount)                      extends TelluricType("UserDefined") derives Eq
  case object NoTelluric                                             extends TelluricType("NoTelluric")

  object ExplicitSpectralTypes:
    val starTypes: Lens[ExplicitSpectralTypes, NonEmptyList[String]] = Focus[ExplicitSpectralTypes](_.starTypes)

  object UserDefined:
    val count: Lens[UserDefined, TelluricCount] = Focus[UserDefined](_.count)

  val hot: Prism[TelluricType, TelluricType.Hot.type] =
    GenPrism[TelluricType, TelluricType.Hot.type]

  val a0v: Prism[TelluricType, TelluricType.A0V.type] =
    GenPrism[TelluricType, TelluricType.A0V.type]

  val solar: Prism[TelluricType, TelluricType.Solar.type] =
    GenPrism[TelluricType, TelluricType.Solar.type]

  val explicitSpectralTypes: Prism[TelluricType, TelluricType.ExplicitSpectralTypes] =
    GenPrism[TelluricType, TelluricType.ExplicitSpectralTypes]

  val userDefined: Prism[TelluricType, TelluricType.UserDefined] =
    GenPrism[TelluricType, TelluricType.UserDefined]

  val none: Prism[TelluricType, TelluricType.NoTelluric.type] =
    GenPrism[TelluricType, TelluricType.NoTelluric.type]
