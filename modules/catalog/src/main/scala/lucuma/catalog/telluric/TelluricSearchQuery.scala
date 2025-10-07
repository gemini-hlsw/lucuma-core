// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import clue.GraphQLOperation
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.JsonObject

/**
 * GraphQL query for searching telluric standard star candidates
 */
object TelluricSearchQuery extends GraphQLOperation[Unit]:
  type Data      = List[TelluricStar]
  type Variables = TelluricSearchInput

  override val document: String =
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

  override val varEncoder: Encoder.AsObject[Variables] =
    Encoder.AsObject.instance[TelluricSearchInput] { input =>
      JsonObject(
        "ra_deg" -> Encoder[Double].apply(input.coordinates.ra.toAngle.toDoubleDegrees),
        "dec_deg" -> Encoder[Double].apply(input.coordinates.dec.toAngle.toSignedDoubleDegrees),
        "duration_hrs" -> Encoder[Double].apply(input.duration.toHours.toDouble),
        "brightest" -> Encoder[BigDecimal].apply(input.brightest),
        "sp_type" -> Encoder[String].apply(input.spType)
      )
    }

  override val dataDecoder: Decoder[List[TelluricStar]] =
    (c: HCursor) => c.downField("search").as[List[TelluricStar]]
