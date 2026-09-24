// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.Monoid
import cats.derived.*
import cats.syntax.monoid.*
import lucuma.core.enums.StepType
import monocle.Focus
import monocle.Lens

/**
 * The steps of a sequence partitioned by kind.  The five digests together
 * cover every step, so their times sum to the sequence time estimate.
 *
 * @param biases    bias steps
 * @param darks     dark steps
 * @param arcs      GCAL arc steps
 * @param flats     GCAL flat steps
 * @param observing every other step: science, acquisition and offsets
 */
case class StepDigests(
  biases:    StepDigest,
  darks:     StepDigest,
  arcs:      StepDigest,
  flats:     StepDigest,
  observing: StepDigest
) derives Eq:

  /** Total time across all buckets. */
  def time: CategorizedTime =
    biases.time |+| darks.time |+| arcs.time |+| flats.time |+| observing.time

  /** Adds one step to the bucket its step type selects. */
  def add[D](s: Step[D]): StepDigests =
    val t = s.timeEstimate
    s.stepConfig.stepType match
      case StepType.Bias                      => copy(biases = biases.add(t))
      case StepType.Dark                      => copy(darks = darks.add(t))
      // A GCAL step is an arc or a flat; an unrecognised lamp counts as a flat.
      case StepType.Gcal | StepType.SmartGcal =>
        if s.stepConfig.isArc then copy(arcs = arcs.add(t)) else copy(flats = flats.add(t))
      case StepType.Science                   => copy(observing = observing.add(t))

object StepDigests:

  val Zero: StepDigests =
    StepDigests(StepDigest.Zero, StepDigest.Zero, StepDigest.Zero, StepDigest.Zero, StepDigest.Zero)

  given Monoid[StepDigests] =
    Monoid.instance(
      Zero,
      (a, b) =>
        StepDigests(
          a.biases    |+| b.biases,
          a.darks     |+| b.darks,
          a.arcs      |+| b.arcs,
          a.flats     |+| b.flats,
          a.observing |+| b.observing
        )
    )

  /** @group Optics */
  val biases: Lens[StepDigests, StepDigest] =
    Focus[StepDigests](_.biases)

  /** @group Optics */
  val darks: Lens[StepDigests, StepDigest] =
    Focus[StepDigests](_.darks)

  /** @group Optics */
  val arcs: Lens[StepDigests, StepDigest] =
    Focus[StepDigests](_.arcs)

  /** @group Optics */
  val flats: Lens[StepDigests, StepDigest] =
    Focus[StepDigests](_.flats)

  /** @group Optics */
  val observing: Lens[StepDigests, StepDigest] =
    Focus[StepDigests](_.observing)
