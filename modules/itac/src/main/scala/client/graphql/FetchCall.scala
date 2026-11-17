// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package client.graphql

import clue.*
import io.circe.Json
import io.circe.JsonObject
import lucuma.core.model.CallForProposals

object FetchCall:

  def apply(cid: CallForProposals.Id): GraphQLOperation.Typed[Nothing, JsonObject, Json] =
    new GraphQLOperation.Typed[Nothing, JsonObject, Json]:
      override val document = GraphQLDocument.unsafeFromString:
        s"""
{
  callForProposals(callForProposalsId: "$cid") {
    title
  }
}
        """