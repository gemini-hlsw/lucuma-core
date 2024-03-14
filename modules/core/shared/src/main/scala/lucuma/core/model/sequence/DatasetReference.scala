// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package sequence


import cats.Order
import cats.parse.Parser
import cats.parse.Parser.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Encoder
import io.circe.syntax.*
import lucuma.core.optics.Format

/**
 * Dataset reference combines the observation reference with the step index
 * and exposure index.
 */
case class DatasetReference(
  observationReference: ObservationReference,
  stepIndex:            PosInt,
  exposureIndex:        PosInt
) {

  /** Formatted dataset reference String. */
  def label: String =
    f"${observationReference.label}-$stepIndex%04d-$exposureIndex%04d"

}

object DatasetReference {

  object parse {
    import ObservationReference.parse.observation
    import parser.ReferenceParsers.*

    val dataset: Parser[DatasetReference] =
      ((observation <* dash) ~ (index <* dash) ~ index).map { case ((obs, step), exposure) =>
        DatasetReference(obs, step, exposure)
      }
  }

  given Order[DatasetReference] =
    Order.by { a => (a.observationReference, a.stepIndex.value, a.exposureIndex.value) }

  given Ordering[DatasetReference] =
    Order[DatasetReference].toOrdering

  val fromString: Format[String, DatasetReference] =
    Format(s => parse.dataset.parseAll(s).toOption, _.label)

  given Decoder[DatasetReference] =
    Decoder.decodeString.emap { s =>
      fromString
        .getOption(s)
        .toRight(s"Could not parse '$s' as a DatasetReference.")
    }

  given Encoder[DatasetReference] =
    Encoder.instance(_.label.asJson)

}

