// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import clue.GraphQLOperation
import io.circe.Decoder

case class TelluricSearchQueryResult(search: List[TelluricStar]) derives Decoder

object TelluricSearchQuery
    extends GraphQLOperation.Typed[TelluricService, TelluricSearchInput, TelluricSearchQueryResult]:

  val document: String =
    """
        query TelluricSearch($ra_deg: Float!, $dec_deg: Float!, $duration_hrs: Float!, $brightest: Float!, $sp_type: String!) {
          search(ra_deg: $ra_deg, dec_deg: $dec_deg, duration_hrs: $duration_hrs, brightest: $brightest, sp_type: $sp_type) {
            HIP
            spType
            RA
            Dec
            Distance
            Hmag
            Score
            Order
          }
        }
      """
