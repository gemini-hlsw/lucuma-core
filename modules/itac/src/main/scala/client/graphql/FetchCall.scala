// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package client.graphql

import clue.*
import io.circe.Json
import io.circe.JsonObject
import lucuma.core.model.Semester

object FetchCall:

  def apply(sem: Semester): GraphQLOperation.Typed[Nothing, JsonObject, Json] =
    new GraphQLOperation.Typed[Nothing, JsonObject, Json]:
      override val document = GraphQLDocument.unsafeFromString:
        s"""
query MyQuery {
  callsForProposals(
    includeDeleted: false
    WHERE: { semester: { EQ: "${sem.format}" }, gemini: { type: { EQ: REGULAR_SEMESTER } } }
  ) {
    matches {
      id
      title
    }
  }
}        """