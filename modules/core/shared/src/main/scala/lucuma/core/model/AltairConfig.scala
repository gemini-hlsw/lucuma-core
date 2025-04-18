// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.all.*
import coulomb.Quantity
import coulomb.units.accepted.Millimeter

sealed trait AltairConfig

object AltairConfig {
  case object AltairOff extends AltairConfig
  case class Ngs(
    blend: Boolean, 
    starPos: (Quantity[BigDecimal, Millimeter], Quantity[BigDecimal, Millimeter])
  ) extends AltairConfig
  case class Lgs(
    strap: Boolean, 
    sfo: Boolean, 
    starPos: (Quantity[BigDecimal, Millimeter], Quantity[BigDecimal, Millimeter])
  ) extends AltairConfig
  case object LgsWithP1 extends AltairConfig
  case object LgsWithOi extends AltairConfig

  given Eq[Ngs] = Eq.by(x => (x.blend, x.starPos._1.value, x.starPos._2.value))
  given Eq[Lgs] = Eq.by(x => (x.strap, x.sfo, x.starPos._1.value, x.starPos._2.value))

  given Eq[AltairConfig] = Eq.instance {
    case (AltairOff, AltairOff) => true
    case (a: Lgs, b: Lgs)       => a === b
    case (a: Ngs, b: Ngs)       => a === b
    case (LgsWithOi, LgsWithOi) => true
    case (LgsWithP1, LgsWithP1) => true
    case _                      => false
  }
}

