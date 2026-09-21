// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ocs

import scala.xml.Elem

object Anonymizer:

  def anonymize(root: Elem): Elem =
    <proposal>
      { root \ "semester" }
      { root \ "targets" }
      { root \ "conditions" }
      { root \ "blueprints" }
      { root \ "observations" }
      { root \ "proposalClass" }
    </proposal>
