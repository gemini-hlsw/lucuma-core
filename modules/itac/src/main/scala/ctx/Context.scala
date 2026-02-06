// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.ctx

import lucuma.core.enums.Site
import lucuma.core.model.Semester

final case class Context(site: Site, semester: Semester) 