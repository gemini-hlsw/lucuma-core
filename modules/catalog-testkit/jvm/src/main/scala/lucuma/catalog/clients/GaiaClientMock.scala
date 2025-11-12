// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.data.NonEmptyChain
import cats.effect.*
import cats.syntax.all.*
import fs2.Stream
import fs2.io.readClassLoaderResource
import fs2.text
import lucuma.catalog.votable.CatalogAdapter
import org.http4s.*
import org.http4s.client.Client

import scala.xml.Node
import scala.xml.Utility
import scala.xml.XML

object GaiaClientMock:
  enum Mode:
    case All, ById

  /**
   * Helper to filter VOTable XML to only include rows matching a specific source ID.
   */
  private def filterVoTableById(voTableXml: String, sourceId: Long): String = {
    import scala.xml._

    val xml      = XML.loadString(voTableXml)
    val targetId = sourceId.toString

    // Find all TR elements and filter to the one containing the target ID
    val filteredRows = (xml \\ "TR").filter: tr =>
      (tr \ "TD").headOption.exists: td =>
        td.text === targetId || td.text.endsWith(targetId)

    val table = xml \\ "TABLE"
    if (filteredRows.isEmpty || table.isEmpty) {
      voTableXml
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
   * ) Create a mock GaiaClient that returns VOTable XML responses.
   */
  def mockGaiaClient[F[_]: Concurrent](
    voTableXml: Stream[F, String],
    adapters:   Option[NonEmptyChain[CatalogAdapter.Gaia]] = None
  ): GaiaClient[F] = {
    val mockHttpClient = Client[F]: request =>
      // Check if contains "WHERE source_id = "
      val query           = request.uri.query.params.getOrElse("QUERY", "")
      val sourceIdPattern = """WHERE source_id = (\d+)""".r

      val responseXml = sourceIdPattern.findFirstMatchIn(query) match {
        case Some(m) =>
          val sourceId = m.group(1).toLong
          voTableXml.map(filterVoTableById(_, sourceId))
        case None    =>
          voTableXml
      }

      Resource.pure(Response(Status.Ok).withEntity(responseXml))

    adapters match {
      case Some(a) => GaiaClient.build[F](mockHttpClient, adapters = a)
      case None    => GaiaClient.build[F](mockHttpClient)
    }
  }

  /**
   * Create a mock GaiaClient that reads XML.
   */
  def fromXML[F[_]: Concurrent](
    xml:      Node,
    adapters: Option[NonEmptyChain[CatalogAdapter.Gaia]]
  ): GaiaClient[F] =
    mockGaiaClient(Stream.emit(Utility.trim(xml).toString), adapters)

  /**
   * Create a mock GaiaClient that reads a String.
   */
  def fromString[F[_]: Concurrent](
    content:  String,
    adapters: Option[NonEmptyChain[CatalogAdapter.Gaia]]
  ): GaiaClient[F] =
    mockGaiaClient(Stream.emit(content), adapters)

  /**
   * Create a mock GaiaClient that reads VOTable XML from a resource file.
   */
  def fromResource[F[_]: Async](
    resource: String,
    adapters: Option[NonEmptyChain[CatalogAdapter.Gaia]]
  ): GaiaClient[F] =
    mockGaiaClient(readClassLoaderResource[F](resource, 8192).through(text.utf8.decode), adapters)
