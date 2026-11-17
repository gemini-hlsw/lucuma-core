// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package client.graphql

import cats.effect.Async
import cats.effect.kernel.Resource
import cats.free.Free
import cats.syntax.all.*
import clue.*
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import edu.gemini.tac.qengine.p1.GroupTree
import edu.gemini.tac.qengine.p1.ItacObservation
import edu.gemini.tac.qengine.p1.Proposal
import io.circe.Decoder
import io.circe.Json
import io.circe.JsonObject
import lucuma.core.model.CallForProposals
import lucuma.core.model.Observation
import lucuma.core.model.Semester
import org.http4s.Headers
import org.http4s.Uri
import org.http4s.headers.Authorization
import org.http4s.jdkhttpclient.JdkHttpClient
import org.typelevel.log4cats.Logger

trait Fetch[F[_]]:
  def fetch: F[List[Proposal]]

object Fetch:
  import FetchDecoder.given

  def apply[F[_]: Async: Logger](uri: Uri, auth: Authorization, sem: Semester): Fetch[F] =
    new Fetch[F]:

      val client: Resource[F, FetchClient[F, Nothing]] = 
        JdkHttpClient.simple[F].map(Http4sHttpBackend[F](_)).evalMap: backend =>
          given Http4sHttpBackend[F] = backend
          Http4sHttpClient.of[F, Nothing](uri, headers = Headers(auth))

      def fetch: F[List[Proposal]] =
        for
          cid <- fetchCall(sem)
          _   <- Logger[F].info("Fetching SUBMITTED proposals...")
          eps <- fetchProposals(cid)
          _   <- eps.collect { case Left(s) => s } .traverse(Logger[F].warn(_))
          ps   = eps.collect { case Right(a) => a }
          _   <- Logger[F].info("Fetching DEFINED observations...")
          eos <- fetchObservations(cid)
          _   <- eos.collect { case (id, Left(a)) => (id, a) } .traverse(x => Logger[F].warn(x._2))
          os  = eos.collect { case (id, Right(a)) => (id, a) }
          _   <- Logger[F].info(s"Fetched ${os.length} observations in ${ps.length} proposals.")
          ret  = stitch(ps, os)
          _   <- ret.filter(_.originalObsList.isEmpty).traverse(p => Logger[F].warn(s"${p.reference.label}: proposal has no defined observations, skipping"))
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

      def fetchCall(sem: Semester): F[CallForProposals.Id] =
        fetchJson(FetchCall(sem)).flatMap: json =>
          val hc = json.hcursor.downFields("callsForProposals", "matches").downArray
          val res =
            for
              title <- hc.downField("title").as[String]
              id    <- hc.downField("id").as[CallForProposals.Id]
            yield (title, id) 
          res.toOption match
            case Some((title, cid)) => 
              Logger[F].info(s"Found CFP for ${sem.format}: \"$title\"").as(cid)
            case None => Async[F].raiseError(new RuntimeException(s"No CFP was found for semester ${sem.format}"))
          
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

