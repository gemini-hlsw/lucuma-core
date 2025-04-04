// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.data

import eu.timepit.refined.string.MatchesRegex
import lucuma.core.util.NewRefined

// N.B. this is the same pattern used in the database constraint; if we change one we need to change the other.
type EmailPred =
  MatchesRegex["""^[a-zA-Z0-9_+&-]+(?:.[a-zA-Z0-9_+&-]+)*@(?:[a-zA-Z0-9-]+.)+[a-zA-Z]{2,7}$"""]

object EmailAddress extends NewRefined[String, EmailPred]
type EmailAddress = EmailAddress.Type

extension (self: EmailAddress) def value: EmailAddress = self

