// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import monocle.Iso
import monocle.Prism
import monocle.macros.GenPrism

/**
 * Observations with a mask to be defined are valid.
 * We'll represent that state with the ADT below
 */
sealed trait MaskDefinition derives Eq

case object ToBeDefined extends MaskDefinition derives Eq
case class Defined(id: Attachment.Id) extends MaskDefinition derives Eq

object Defined:
  def id: Iso[Defined, Attachment.Id] =
    Iso[Defined, Attachment.Id](_.id)(Defined(_))

object MaskDefinition:
  val toBeDefined: Prism[MaskDefinition, ToBeDefined.type] =
    GenPrism[MaskDefinition, ToBeDefined.type]

  val defined: Prism[MaskDefinition, Defined] =
    GenPrism[MaskDefinition, Defined]
