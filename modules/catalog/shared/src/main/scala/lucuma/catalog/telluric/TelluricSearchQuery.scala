// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import clue.GraphQLOperation
import io.circe.Decoder
import io.circe.Encoder
import io.circe.JsonObject
import io.circe.syntax.*
import lucuma.catalog.telluric.codecs.given

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
    Encoder.AsObject.instance[TelluricSearchInput]: input =>
      JsonObject(
        "ra_deg"       -> input.coordinates.ra.toAngle.toDoubleDegrees.asJson,
        "dec_deg"      -> input.coordinates.dec.toAngle.toSignedDoubleDegrees.asJson,
        "duration_hrs" -> input.duration.toHours.toDouble.asJson,
        "brightest"    -> input.brightest.asJson,
        "sp_type"      -> input.spType.asJson
      )

  override val dataDecoder: Decoder[List[TelluricStar]] =
    _.downField("search").as[List[TelluricStar]]
