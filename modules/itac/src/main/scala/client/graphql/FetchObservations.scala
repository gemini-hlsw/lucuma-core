// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package client.graphql

import clue.*
import io.circe.Json
import io.circe.JsonObject
import lucuma.core.model.CallForProposals

object FetchObservations:

  def apply(cid: CallForProposals.Id): GraphQLOperation.Typed[Nothing, JsonObject, Json] =
    new GraphQLOperation.Typed[Nothing, JsonObject, Json]:
      override val document = GraphQLDocument.unsafeFromString:
        s"""
          query {
            observations(
              includeDeleted: false
              WHERE: {
                calibrationRole: { IS_NULL: true }
                workflow: { workflowState: { EQ: DEFINED } }
                program: { proposal: { call: { id: { EQ: "${cid}" } } } }
              }
            ) {
              matches {
                id
                configuration {
                  conditions {
                    cloudExtinction
                    imageQuality
                    skyBackground
                    waterVapor
                  }
                  observingMode {
                    mode
                  }
                  target {
                    coordinates {
                      dec {
                        dms
                      }
                      ra {
                        hms
                      }
                    }
                  }
                  altairMode
                }
                execution {
                  digest {
                    value {
                      fullTimeEstimate {
                        program {
                          microseconds
                        }
                      }
                    }
                  }
                }
                id
                workflow {
                  value {
                    state
                  }
                }
              }
            }
          }
        """