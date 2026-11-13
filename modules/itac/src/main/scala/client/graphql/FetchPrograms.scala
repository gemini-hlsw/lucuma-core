// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package client.graphql

import clue.*
import io.circe.Json
import io.circe.JsonObject
import lucuma.core.model.CallForProposals

object FetchPrograms:

  def apply(cid: CallForProposals.Id): GraphQLOperation.Typed[Nothing, JsonObject, Json] =
    new GraphQLOperation.Typed[Nothing, JsonObject, Json]:
      override val document = GraphQLDocument.unsafeFromString:
        s"""
{
  programs(
    includeDeleted: false
    WHERE: {
      proposalStatus: { EQ: SUBMITTED }
      proposal: { call: { id: { EQ: "$cid" } } }
    }
  ) {
    matches {
      name
      proposalStatus
      maxTooActivation
      pi {
        user {
          profile {
            familyName
            email
          }
        }
      }
      proposal {
        reference {
          label
        }
        gemini {
          scienceSubtype
        }
        call {
          active {
            end
            start
          }
        }
      }
      allocations {
        category
        duration {
          microseconds
        }
        scienceBand
      }
      allGroupElements(includeDeleted: false) {
        group {
          minimumRequired
          parentId
          id
        }
        observation {
          id
          groupId
        }
      }
    }
  }
}


        """