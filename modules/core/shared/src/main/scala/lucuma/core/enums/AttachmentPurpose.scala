// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.Eq
import cats.derived.*

enum AttachmentPurpose derives Eq:
  case Proposal    extends AttachmentPurpose
  case Observation extends AttachmentPurpose
  case Target      extends AttachmentPurpose
