// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.log.RejectMessage


class TimeResourceGroup(val lst: List[TimeResource]) extends Resource {
  type T = TimeResourceGroup

  def reserve(block: Block, queue: ProposalQueueBuilder): RejectMessage Either TimeResourceGroup =
    Resource.reserveAll(block, queue, lst) map {
      lst => new TimeResourceGroup(lst)
    }

}