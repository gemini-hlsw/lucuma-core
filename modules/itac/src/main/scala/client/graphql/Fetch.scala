// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package client.graphql

import clue.*
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import org.http4s.Uri
import org.typelevel.log4cats.Logger
import cats.effect.Async
import cats.effect.kernel.Resource
import org.http4s.headers.Authorization
import org.http4s.Headers
import org.http4s.jdkhttpclient.JdkHttpClient
import edu.gemini.tac.qengine.p1.Proposal
import io.circe.Decoder
import cats.syntax.all.* 
import lucuma.core.model.CallForProposals
import io.circe.Json
import io.circe.JsonObject
import edu.gemini.tac.qengine.p1.ItacObservation
import lucuma.core.model.Observation
import edu.gemini.tac.qengine.p1.GroupTree
import cats.free.Free

trait Fetch[F[_]]:
  def fetch: F[List[Proposal]]

object Fetch:
  import FetchDecoder.given

  def apply[F[_]: Async: Logger](uri: Uri, auth: Authorization, callId: CallForProposals.Id): Fetch[F] =
    new Fetch[F]:

      val client: Resource[F, FetchClient[F, Nothing]] = 
        JdkHttpClient.simple[F].map(Http4sHttpBackend[F](_)).evalMap: backend =>
          given Http4sHttpBackend[F] = backend
          Http4sHttpClient.of[F, Nothing](uri, headers = Headers(auth))

      def fetch: F[List[Proposal]] =
        for
          _  <- fetchCall(callId)
          _  <- Logger[F].info("Fetching SUBMITTED proposals...")
          eps <- fetchProposals(callId)
          _   <- eps.collect { case Left(s) => s } .traverse(Logger[F].warn(_))
          ps   = eps.collect { case Right(a) => a }
          _   <- Logger[F].info("Fetching DEFINED observations...")
          eos <- fetchObservations(callId)
          _   <- eos.collect { case (id, Left(a)) => (id, a) } .traverse(x => Logger[F].warn(x._2))
          os  = eos.collect { case (id, Right(a)) => (id, a) }
          _   <- Logger[F].info(s"Fetched ${os.length} observations in ${ps.length} proposals.")
          ret  = stitch(ps, os)
          _   <- ret.filter(_.originalObsList.isEmpty).traverse(p => Logger[F].warn(s"${p.reference.label}: no DEFINED observations"))
        yield ret.filter(_.originalObsList.nonEmpty)

      def stitch(ps: List[(Proposal, GroupTree[Observation.Id])], os: List[(Observation.Id, ItacObservation)]): List[Proposal] =
        val omap = os.toMap
        ps.map: (p, gt) =>
          p.copy(
            groupTree =
              gt.map(omap.get).flatMap:
                case Some(o) => Free.pure(o)
                case None    => GroupTree.empty[ItacObservation]
          )

      def fetchCall(cid: CallForProposals.Id): F[Unit] =
        fetchJson(FetchCall(cid)).flatMap: json =>
          val hc = json.hcursor.downFields("callForProposals")
          hc.downField("title").as[String] match
            case Left(_) => Logger[F].error(s"No such call for proposals: $cid")
            case Right(t) => Logger[F].info(s"Found CFP: \"$t\"")          

      def fetchProposals(cid: CallForProposals.Id): F[List[Either[String, (Proposal, GroupTree[Observation.Id])]]] =
        fetchJson(FetchPrograms(cid)).map: json =>
          json.hcursor.downFields("programs", "matches").as[List[Either[String, (Proposal, GroupTree[Observation.Id])]]] match
            case Right(ps) => ps
            case Left(e) => throw new RuntimeException(e.toString)

      def fetchObservations(cid: CallForProposals.Id): F[List[(Observation.Id, Either[String, ItacObservation])]] =
        fetchJson(FetchObservations(cid)).map: json =>
          json.hcursor.downFields("observations", "matches").as[List[(Observation.Id, Either[String, ItacObservation])]] match
            case Right(res) => res
            case Left(e) => throw new RuntimeException(e.toString)

      def fetchJson(op: GraphQLOperation.Typed[Nothing, JsonObject, Json] ): F[Json] =
        client.use: fc =>
          fc.request(op, None).apply.map: res =>
            res.data match
              case Some(json) => json
              case None =>
                throw new RuntimeException(s"No data received: ${res.errors}")          

