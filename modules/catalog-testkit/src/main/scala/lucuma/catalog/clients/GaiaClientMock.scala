// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.data.NonEmptyChain
import cats.effect.*
import cats.syntax.all.*
import lucuma.catalog.votable.CatalogAdapter
import org.http4s.*
import org.http4s.client.Client

import scala.xml.Utility

object GaiaClientMock:

  /**
   * Helper to filter VOTable XML to only include rows matching a specific source ID. Handles both
   * prefixed ("Gaia DR2 123") and raw numeric ("123") formats.
   */
  def filterVoTableById(voTableXml: String, sourceId: Long): String = {
    import scala.xml._

    val xml      = XML.loadString(voTableXml)
    val targetId = sourceId.toString

    // Find all TR elements and filter to the one containing the target ID
    // Handle both prefixed ("Gaia DR2 123") and raw numeric ("123") formats
    val filteredRows = (xml \\ "TR").filter { tr =>
      (tr \ "TD").headOption.exists { td =>
        td.text == targetId || td.text.endsWith(targetId)
      }
    }

    // Reconstruct the VOTable with only the matching row
    val table = xml \\ "TABLE"
    if (filteredRows.isEmpty || table.isEmpty) {
      voTableXml // Return original if no match found
    } else {
      val fields        = table.head \ "FIELD"
      val filteredTable =
        <TABLE>
          {fields}
          <DATA>
            <TABLEDATA>
              {filteredRows}
            </TABLEDATA>
          </DATA>
        </TABLE>

      Utility.trim(filteredTable).toString
    }
  }

  /**
   * Create a mock GaiaClient that returns VOTable XML responses. Supports filtering for queryById
   * requests based on ADQL query inspection.
   *
   * @param voTableXml
   *   The VOTable XML to return in responses
   * @param adapters
   *   Optional specific adapters to use (default: all Gaia adapters)
   */
  def mockGaiaClient[F[_]: Async](
    voTableXml: String,
    adapters:   Option[NonEmptyChain[CatalogAdapter.Gaia]] = None
  ): GaiaClient[F] = {
    val mockHttpClient = Client.fromHttpApp[F](HttpApp[F] { request =>
      // Check if this is a queryById request (contains "WHERE source_id = ")
      val query           = request.uri.query.params.get("QUERY").getOrElse("")
      val sourceIdPattern = """WHERE source_id = (\d+)""".r

      val responseXml = sourceIdPattern.findFirstMatchIn(query) match {
        case Some(m) =>
          val sourceId = m.group(1).toLong
          filterVoTableById(voTableXml, sourceId)
        case None    =>
          voTableXml
      }

      Response[F](Status.Ok).withEntity(responseXml).pure[F]
    })

    adapters match {
      case Some(adpts) => GaiaClient.build[F](mockHttpClient, adapters = adpts)
      case None        => GaiaClient.build[F](mockHttpClient)
    }
  }
