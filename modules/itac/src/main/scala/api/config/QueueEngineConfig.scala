// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

/**
 * A combination of configuration required by the Queue Engine.
 */
final case class QueueEngineConfig(
  binConfig:           SiteSemesterConfig,
  TimeAccountingCategorySeq:          TimeAccountingCategorySequence,
  restrictedBinConfig: RestrictionConfig = RestrictionConfig(),
) {
  def site = binConfig.site
}
