// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.foldable.*
import monocle.Focus
import monocle.Lens

/**
 * ExecutionConfig groups the static configuration with the acquisition and
 * science sequences.
 *
 * @tparam S instrument static configuration type (e.g., `gmos.StaticConfig.GmosNorth`)
 * @tparam D instrument dynamic configuration type (e.g., `gmos.DynamicConfig.GmosNorth`)
 */
case class ExecutionConfig[S, D](
  static:        S,
  acquisition:   Option[ExecutionSequence[D]],
  science:       Option[ExecutionSequence[D]]
):
  /** Returns `true` if there are no science steps to execute. */
  def isComplete: Boolean =
    science.isEmpty

object ExecutionConfig:
  /** Shorthand type for GmosNorth. */
  type GmosNorth = ExecutionConfig[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth]
  /** Shorthand type for GmosSouth. */
  type GmosSouth = ExecutionConfig[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth]

  given [S, D](using Eq[S], Eq[D]): Eq[ExecutionConfig[S, D]] =
    Eq.by { a => (
      a.static,
      a.acquisition,
      a.science
    )}

  /** @group Optics */
  def static[S, D]: Lens[ExecutionConfig[S, D], S] =
    Focus[ExecutionConfig[S, D]](_.static)

  /** @group Optics */
  def acquisition[S, D]: Lens[ExecutionConfig[S, D], Option[ExecutionSequence[D]]] =
    Focus[ExecutionConfig[S, D]](_.acquisition)

  /** @group Optics */
  def science[S, D]: Lens[ExecutionConfig[S, D], Option[ExecutionSequence[D]]] =
    Focus[ExecutionConfig[S, D]](_.science)