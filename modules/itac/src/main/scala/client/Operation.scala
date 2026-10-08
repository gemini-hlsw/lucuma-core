// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac

import cats.effect.ExitCode
import org.typelevel.log4cats.Logger

trait Operation[F[_]] {
  def run(ws: Workspace[F], log: Logger[F]): F[ExitCode]
}
