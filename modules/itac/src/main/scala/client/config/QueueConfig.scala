// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac
package config

import cats.syntax.all.*
import edu.gemini.qengine.skycalc.DecBinSize
import edu.gemini.qengine.skycalc.RaBinSize
import edu.gemini.tac.qengine.api.queue.time.QueueTime
import edu.gemini.tac.qengine.api.queue.time.TimeAccountingCategoryTime
import io.circe.*
import io.circe.generic.semiauto.*
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.IntCentiPercent
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan

// queue configuration
final case class QueueConfig(
  site:       Site,
  overfill:   Map[ScienceBand, IntCentiPercent],
  raBinSize:  RaBinSize,
  decBinSize: DecBinSize,
  hours:      Map[TimeAccountingCategory, BandTimes],
) {

  // Ensure that we don't allocate time for partners at the wrong site.
  Enumerated[TimeAccountingCategory].all.filterNot(_.sites(site)).foreach { p =>
    if (hours.contains(p))
      throw new ItacException(s"Time accounting category ${p.tag.toUpperCase} (p.description) cannot be given time at $site! Please remove this from your queue configuration.")
  }

  object engine {

    val queueTimes: ScienceBand => QueueTime = { b =>

      val pt = TimeAccountingCategoryTime.fromFunction { p =>
        hours.get(p) match {
          case Some(BandTimes(b1, b2, b3, b4)) =>
             b match {
               case ScienceBand.Band1 => b1
               case ScienceBand.Band2 => b2
               case ScienceBand.Band3 => b3
               case ScienceBand.Band4 => b4
             }
          case None    => TimeSpan.Zero
        }
      }

      new QueueTime(pt, overfill.getOrElse(b, IntCentiPercent.Min))

    }

  }

}

object QueueConfig {
  import itac.codec.intcentipercent.given

  given [A](using e: Enumerated[A]): KeyDecoder[A] =
    KeyDecoder.instance(e.fromTag)

  implicit val DecoderQueue: Decoder[QueueConfig] = deriveDecoder
}

case class BandTimes(band1: TimeSpan, band2: TimeSpan, band3: TimeSpan, band4: TimeSpan)
object BandTimes {

  private def t(s: String): Either[String, TimeSpan] =
    Either.catchOnly[NumberFormatException](s.toDouble)
      .leftMap(_.getMessage)
      .map(TimeSpan.fromHoursBounded)

  implicit val DecoderBandTimes: Decoder[BandTimes] =
    Decoder[String].emap { s =>
      s.trim.split("\\s+") match {
        case Array(b1, b2, b3, b4) => (t(b1), t(b2), t(b3), t(b4)).mapN(BandTimes.apply)
        case _ => Left("BandTimes: expected four numbers, like 23.8  23.8  15.9 22.3")
      }
    }

}