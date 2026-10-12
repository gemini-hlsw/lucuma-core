// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package client.operation

import itac.Operation
import itac.Workspace
import org.typelevel.log4cats.Logger
import cats.effect.ExitCode
import lucuma.core.model.ProposalReference
import cats.effect.Sync
import cats.syntax.all.* 

object Info:

  def apply[F[_]: Sync](ref: ProposalReference): Operation[F] =
    new Operation[F]:
      def run(ws: Workspace[F], log: Logger[F]): F[ExitCode] =
        ws.proposal(ref).flatMap: p =>
          val ss = p.shards
          Sync[F].blocking:
            println()
            println(f"${p.reference.label} - ${p.tpe.scienceSubtype} Proposal ${Some(ss.length).filter(_ > 1).foldMap(n => s"with $n shards")} - Ranking ${p.ranking.value}%2.1f")  
            ss.zipWithIndex.foreach: (s, n) =>
              println()
              println(f"  ${Option.when(ss.length > 1)(s"Shard ${n+1} at ").orEmpty}${s.site}, ${s.allocation.duration.toHours}%2.2f h available, awarded by ${s.allocation.category.description}")
              println()
              s.observations.foreach: o =>
                println(f"    ${o.time.toHours}%2.2f h, ${o.mode.tag}, ${o.itacTarget}")
            println()
            ExitCode.Success
 
