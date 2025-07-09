// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import lucuma.core.enums.GcalLampType
import lucuma.core.enums.ObserveClass
import monocle.Focus
import monocle.Lens

/**
 * Atom summary attributes.
 */
case class AtomDigest(
  id:           Atom.Id,
  observeClass: ObserveClass,
  timeEstimate: CategorizedTime,
  hasArc:       Boolean,
  hasFlat:      Boolean
)

object AtomDigest:

  def fromAtom[D](atom: Atom[D]): AtomDigest =
    val lampTypes = atom.steps.foldLeft(Set.empty[GcalLampType]): (types, step) =>
      step.stepConfig.gcalLampType.fold(types)(types + _)

    AtomDigest(
      atom.id,
      atom.observeClass,
      atom.timeEstimate,
      lampTypes(GcalLampType.Arc),
      lampTypes(GcalLampType.Flat)
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
  val hasArc: Lens[AtomDigest, Boolean] =
    Focus[AtomDigest](_.hasArc)

  /** @group Optics */
  val hasFlat: Lens[AtomDigest, Boolean] =
    Focus[AtomDigest](_.hasFlat)

  given Eq[AtomDigest] =
    Eq.by: a =>
      (
        a.id,
        a.observeClass,
        a.timeEstimate,
        a.hasArc,
        a.hasFlat
      )