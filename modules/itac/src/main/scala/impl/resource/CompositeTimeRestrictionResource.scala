// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.log.RejectMessage


class CompositeTimeRestrictionResource(val lst: List[TimeRestrictionResource]) extends Resource {
  type T = CompositeTimeRestrictionResource

  def reserve(block: Block, queue: ProposalQueueBuilder): RejectMessage Either CompositeTimeRestrictionResource =
    Resource.reserveAll(block, queue, lst) map {
      lst => new CompositeTimeRestrictionResource(lst)
    }

}