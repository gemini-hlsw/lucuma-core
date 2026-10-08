// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.util

import scala.io.AnsiColor

/** Like `AnsiColor` but sets colors to "" if there's no console attached. */
object Colors {

  private lazy val isDetached = {
    (!sys.props.contains("force-color")) && (System.console == null)
  }

  private def color(s: String): String =
    if (isDetached) "" else s

  lazy val BOLD   = color(AnsiColor.BOLD)
  lazy val RESET  = color(AnsiColor.RESET)
  lazy val RED    = color(AnsiColor.RED)
  lazy val YELLOW = color(AnsiColor.YELLOW)
  lazy val GREEN  = color(AnsiColor.GREEN)
  lazy val BLUE   = color(AnsiColor.BLUE)
  lazy val CYAN   = color(AnsiColor.CYAN)

}