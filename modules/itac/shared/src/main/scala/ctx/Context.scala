// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.ctx

import edu.gemini.spModel.core.Site
import edu.gemini.spModel.core.Semester

final case class Context(site: Site, semester: Semester)