// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.operation

import cats._
import cats.effect.ExitCode
import cats.implicits._
import itac.Workspace
import itac.Operation
import cats.effect.Sync
import cats.data.NonEmptyList
import itac.ItacException
import itac.util.Colors
import org.typelevel.log4cats.Logger
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.util.Enumerated
import edu.gemini.tac.qengine.p1.ProposalShard

object Ls {

  def apply[F[_]: Sync](fields: NonEmptyList[Field], partnerNames: List[String]): Operation[F] =
    new Operation[F] {

      val order = fields.reduceMap(_.order)(using Order.whenEqualMonoid)

      def header: String =
        f"${Colors.BOLD}${"Id"}%-25s  Site  ${"PI"}%-20s   ${"Rank"}%4s ${"Partner"}%6s   ${"Time"}%6s${Colors.RESET}"

      def format(s: ProposalShard): String =
        f"${s.reference}%-25s  ${s.site.shortName}    ${"todo-pi name"}%-20s  ${s.parentProposal.ranking.value}%5.2f  ${s.allocation.category.tag.toUpperCase}%6s  ${s.allocation.duration.toHours}%5.1f h"

      def findPartner(name: String): F[TimeAccountingCategory] =
        Enumerated[TimeAccountingCategory].all.find(_.tag.equalsIgnoreCase(name)) match {
          case Some(p) => p.pure[F]
          case None    => new ItacException(s"No such partner: $name. Try one or more of ${Enumerated[TimeAccountingCategory].all.map(_.tag).mkString(",")}").raiseError[F, TimeAccountingCategory]
        }

      def run(ws: Workspace[F], log: Logger[F]): F[ExitCode] = {
        for {
          ns <- if (partnerNames.isEmpty) Enumerated[TimeAccountingCategory].all.pure[F]
                else partnerNames.traverse(findPartner)
          ss <- ws.shards.map(_.filter(s => ns.contains(s.allocation.category)).sorted(using order.toOrdering))
          _  <- Sync[F].delay(println(header))
          _  <- ss.traverse_(s => Sync[F].delay(println(format(s))))
        } yield ExitCode.Success
      }

  }

  final case class Field(name: String, order: Order[ProposalShard])
  object Field {

    val id      = Field("id",       Order.by(_.reference))
    val site    = Field("site",     Order.by(_.site.shortName))
    val pi      = Field("pi",       Order.by(_ => "todo: partner"))
    val rank    = Field("rank",     Order.by(_.parentProposal.ranking.value))
    val partner = Field("partner",  Order.by(_.allocation.category))
    val time    = Field("time",     Order.by(_.allocation.duration))

    val all: List[Field] =
      List(id, site, pi, rank, partner, time)

    def fromString(name: String): Either[String, Field] =
      all.find(_.name.toLowerCase == name)
         .toRight(s"No such field: $name. Try one or more of ${all.map(_.name).mkString(",")}")

    def parse(ss: String): Either[String, NonEmptyList[Field]] =
      ss.split(",")
        .toList
        .traverse(fromString)
        .flatMap { fs =>
          NonEmptyList
            .fromList(fs)
            .toRight(s"No fields specified. Try one or more of ${all.map(_.name).mkString(",")}")
        }

  }

}