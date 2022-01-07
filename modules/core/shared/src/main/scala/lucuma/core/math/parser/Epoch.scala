// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.parser

import atto._
import atto.syntax.refined._
import cats.syntax.all._
import lucuma.core.math.Epoch
import lucuma.core.math.Epoch.Julian
import lucuma.core.parser.MiscParsers

import Atto._

/** Parser for [[lucuma.core.math.Epoch]]. */
trait EpochParsers {
  import MiscParsers.intN

  val besselian: Parser[Epoch.Scheme] =
    char('B').as[Epoch.Scheme](Epoch.Besselian).named("besselian")

  val julian: Parser[Epoch.Scheme] =
    char('J').as[Epoch.Scheme](Epoch.Julian).named("julian")

  /** Parser for an `Epoch.Scheme`. */
  val epochScheme: Parser[Epoch.Scheme] =
    (besselian | julian).named("epochScheme")

  /** Parser for an `Epoch`. */
  val epoch: Parser[Epoch] =
    (epochScheme, int.refined[Epoch.Year] <~ char('.'), intN(3))
      .mapN { (s, y, f) =>
        s.fromMilliyearsUnsafe(y.value * 1000 + f)
      }
      .named("epoch")

  /** Parser for an `Epoch` with a default Julian schemme. */
  val epochLenientNoScheme: Parser[Epoch] =
    (int.refined[Epoch.Year] <~ opt(char('.')), MiscParsers.frac(3))
      .mapN {
        case (y, f) =>
          Julian.fromMilliyearsUnsafe(y.value * 1000 + f)
      }
      .named("julianEpoch")

}

object EpochParsers extends EpochParsers
