// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Show
import cats.syntax.all.*
import lucuma.core.enums.*

sealed trait GemsConfig extends Product with Serializable {
    val isCwfs1Used: Boolean
    val isCwfs2Used: Boolean
    val isCwfs3Used: Boolean
    val isOdgw1Used: Boolean
    val isOdgw2Used: Boolean
    val isOdgw3Used: Boolean
    val isOdgw4Used: Boolean
    val isP1Used: Boolean
    val isOIUsed: Boolean
  }

object GemsConfig {
  case object GemsOff extends GemsConfig {
    override val isCwfs1Used: Boolean = false
    override val isCwfs2Used: Boolean = false
    override val isCwfs3Used: Boolean = false
    override val isOdgw1Used: Boolean = false
    override val isOdgw2Used: Boolean = false
    override val isOdgw3Used: Boolean = false
    override val isOdgw4Used: Boolean = false
    override val isP1Used: Boolean    = false
    override val isOIUsed: Boolean    = false
  }

  final case class GemsOn(
    cwfs1: Cwfs1Usage,
    cwfs2: Cwfs2Usage,
    cwfs3: Cwfs3Usage,
    odgw1: Odgw1Usage,
    odgw2: Odgw2Usage,
    odgw3: Odgw3Usage,
    odgw4: Odgw4Usage,
    useP1: P1Usage,
    useOI: OIUsage
  ) extends GemsConfig {

    override val isCwfs1Used: Boolean = cwfs1 === Cwfs1Usage.Use

    override val isCwfs2Used: Boolean = cwfs2 === Cwfs2Usage.Use

    override val isCwfs3Used: Boolean = cwfs3 === Cwfs3Usage.Use

    override val isOdgw1Used: Boolean = odgw1 === Odgw1Usage.Use

    override val isOdgw2Used: Boolean = odgw2 === Odgw2Usage.Use

    override val isOdgw3Used: Boolean = odgw3 === Odgw3Usage.Use

    override val isOdgw4Used: Boolean = odgw4 === Odgw4Usage.Use

    override val isP1Used: Boolean = useP1 === P1Usage.Use

    override val isOIUsed: Boolean = useOI === OIUsage.Use
  }

  given Show[GemsConfig] = Show.show { x =>
    List(
      if (x.isCwfs1Used) "CWFS1".some else none,
      if (x.isCwfs2Used) "CWFS2".some else none,
      if (x.isCwfs3Used) "CWFS3".some else none,
      if (x.isOdgw1Used) "ODGW1".some else none,
      if (x.isOdgw2Used) "ODGW2".some else none,
      if (x.isOdgw3Used) "ODGW3".some else none,
      if (x.isOdgw4Used) "ODGW4".some else none
    ).flattenOption
      .mkString("(", ", ", ")")
  }

  given Eq[GemsOn] = Eq.by(x =>
    (
      x.cwfs1,
      x.cwfs2,
      x.cwfs3,
      x.odgw1,
      x.odgw2,
      x.odgw3,
      x.odgw4,
      x.useP1,
      x.useOI
    )
  )

  given Eq[GemsConfig] = Eq.instance {
    case (a: GemsOn, b: GemsOn) => a === b
    case (GemsOff, GemsOff)     => true
    case _                      => false
  }
}

