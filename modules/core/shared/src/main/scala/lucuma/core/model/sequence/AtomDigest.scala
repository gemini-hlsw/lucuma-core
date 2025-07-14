// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import lucuma.core.enums.GcalLampType
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepType
import monocle.Focus
import monocle.Lens

/**
 * Atom summary attributes.
 */
case class AtomDigest(
  id:           Atom.Id,
  observeClass: ObserveClass,
  timeEstimate: CategorizedTime,
  stepTypes:    Set[StepType],
  lampTypes:    Set[GcalLampType]
)

object AtomDigest:

  def fromAtom[D](atom: Atom[D]): AtomDigest =
    val stepTypes = atom.steps.foldLeft(Set.empty[StepType]): (types, step) =>
      types + step.stepConfig.stepType

    val lampTypes = atom.steps.foldLeft(Set.empty[GcalLampType]): (types, step) =>
      step.stepConfig.gcalLampType.fold(types)(types + _)

    AtomDigest(
      atom.id,
      atom.observeClass,
      atom.timeEstimate,
      stepTypes,
      lampTypes
    )

  /** @group Optics */
  val id: Lens[AtomDigest, Atom.Id] =
    Focus[AtomDigest](_.id)

  /** @group Optics */
  val observeClass: Lens[AtomDigest, ObserveClass] =
    Focus[AtomDigest](_.observeClass)

  /** @group Optics */
  val timeEstimate: Lens[AtomDigest, CategorizedTime] =
    Focus[AtomDigest](_.timeEstimate)

  /** @group Optics */
  val stepTypes: Lens[AtomDigest, Set[StepType]] =
    Focus[AtomDigest](_.stepTypes)

  /** @group Optics */
  val lampTypes: Lens[AtomDigest, Set[GcalLampType]] =
    Focus[AtomDigest](_.lampTypes)

  given Eq[AtomDigest] =
    Eq.by: a =>
      (
        a.id,
        a.observeClass,
        a.timeEstimate,
        a.stepTypes,
        a.lampTypes
      )