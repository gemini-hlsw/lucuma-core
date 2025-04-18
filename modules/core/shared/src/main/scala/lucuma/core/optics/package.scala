// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.Validate
import eu.timepit.refined.refineV
import monocle.Iso
import monocle.Prism

/** Prism from `A` into `A` with refined predicate `P`. */
def refinedPrism[A, P](implicit v: Validate[A, P]): Prism[A, A Refined P] =
  Prism[A, A Refined P](i => refineV[P](i).toOption)(_.value)

/** Iso for coulomb quantities */
def quantityIso[N, U] = Iso[Quantity[N, U], N](_.value)(_.withUnit[U])

sealed trait IsValid[+E]:
  def mapError[E0](f: E => E0): IsValid[E0]

object IsValid:
  case object Valid extends IsValid[Nothing]:
    override def mapError[E0](f: Nothing => E0): IsValid[E0] = this

  case class Invalid[E](error: E) extends IsValid[E]:
    override def mapError[E0](f: E => E0): IsValid[E0] = Invalid(f(error))
