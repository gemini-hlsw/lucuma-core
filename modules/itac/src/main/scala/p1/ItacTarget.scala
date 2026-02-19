// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import lucuma.core.math.Coordinates
import lucuma.core.model.Target

case class ItacTarget(coords: Coordinates, id: Target.Id):
  def ra = coords.ra
  def dec = coords.dec
