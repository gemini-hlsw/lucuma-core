// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum EmailStatus(val tag: String, val description: String):
  case Queued extends EmailStatus("queued", "Mail provider has queued the message.")
  case Rejected extends EmailStatus("rejected", "Mail provider has rejected the message.")
  case Accepted extends EmailStatus("accepted", "Mail provider has accepted the message.")
  case Delivered extends EmailStatus("delivered", "The message has been accepted by the recipient email server.")
  case PermanentFailure extends EmailStatus("permanent_failure", "The message is undeliverable.")
  case TemporaryFailure extends EmailStatus("temporary_failure", "The message could not be delivered, but may be deliverable later.")

object EmailStatus:
  given Enumerated[EmailStatus] = 
    Enumerated.from(
      Queued,
      Rejected,
      Accepted,
      Delivered,
      PermanentFailure,
      TemporaryFailure
    ).withTag(_.tag)
