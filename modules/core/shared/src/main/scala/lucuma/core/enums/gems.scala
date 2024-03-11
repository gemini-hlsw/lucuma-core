// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.NewType

object Cwfs1Usage extends NewType[Boolean] {
  val Use = Cwfs1Usage(true)
  val DontUse = Cwfs1Usage(false)

  def fromBoolean(b: Boolean): Cwfs1Usage = if (b) Use else DontUse
}

type Cwfs1Usage = Cwfs1Usage.Type

object Cwfs2Usage extends NewType[Boolean] {
  val Use = Cwfs2Usage(true)
  val DontUse = Cwfs2Usage(false)

  def fromBoolean(b: Boolean): Cwfs2Usage = if (b) Use else DontUse
}

type Cwfs2Usage = Cwfs2Usage.Type

object Cwfs3Usage extends NewType[Boolean] {
  val Use = Cwfs3Usage(true)
  val DontUse = Cwfs3Usage(false)

  def fromBoolean(b: Boolean): Cwfs3Usage = if (b) Use else DontUse
}

type Cwfs3Usage = Cwfs3Usage.Type

object Odgw1Usage extends NewType[Boolean] {
  val Use = Odgw1Usage(true)
  val DontUse = Odgw1Usage(false)

  def fromBoolean(b: Boolean): Odgw1Usage = if (b) Use else DontUse
}
type Odgw1Usage = Odgw1Usage.Type

object Odgw2Usage extends NewType[Boolean] {
  val Use = Odgw2Usage(true)
  val DontUse = Odgw2Usage(false)

  def fromBoolean(b: Boolean): Odgw2Usage = if (b) Use else DontUse
}
type Odgw2Usage = Odgw2Usage.Type

object Odgw3Usage extends NewType[Boolean] {
  val Use = Odgw3Usage(true)
  val DontUse = Odgw3Usage(false)

  def fromBoolean(b: Boolean): Odgw3Usage = if (b) Use else DontUse
}
type Odgw3Usage = Odgw3Usage.Type

object Odgw4Usage extends NewType[Boolean] {
  val Use = Odgw4Usage(true)
  val DontUse = Odgw4Usage(false)

  def fromBoolean(b: Boolean): Odgw4Usage = if (b) Use else DontUse
}
type Odgw4Usage = Odgw4Usage.Type

object P1Usage extends NewType[Boolean] {
  val Use = P1Usage(true)
  val DontUse = P1Usage(false)

  def fromBoolean(b: Boolean): P1Usage = if (b) Use else DontUse
}
type P1Usage = P1Usage.Type

object OIUsage extends NewType[Boolean] {
  val Use = OIUsage(true)
  val DontUse = OIUsage(false)

  def fromBoolean(b: Boolean): OIUsage = if (b) Use else DontUse
}
type OIUsage = OIUsage.Type
