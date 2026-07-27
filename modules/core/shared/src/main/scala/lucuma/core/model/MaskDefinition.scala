// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*

/**
 * Observations with a mask to be defined are valid.
 * We'll represent that state with the ADT below
 */
sealed trait MaskDefinition derives Eq

case object ToBeDefined extends MaskDefinition derives Eq
case class Defined(id: Attachment.Id) extends MaskDefinition derives Eq
