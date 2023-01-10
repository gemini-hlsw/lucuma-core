// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for dataset QA state.
 * @group Enumerations
 */
sealed abstract class StepQaState(
  val tag:  String
) extends Product with Serializable

object StepQaState {
  /** @group Constructors */ case object Pass   extends StepQaState("Pass")
  /** @group Constructors */ case object Fail   extends StepQaState("Fail")

  implicit val StepQaStateEnumerated: Enumerated[StepQaState] =
    Enumerated.from(Pass, Fail).withTag(_.tag)
}
