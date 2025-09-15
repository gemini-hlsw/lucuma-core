// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import cats.Show
import cats.syntax.option.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Site
import lucuma.core.refined.auto.*
import lucuma.core.util.WithGid
import monocle.Prism
import org.typelevel.cats.time.*

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.control.Exception.allCatch
import scala.util.matching.Regex

object Dataset extends WithGid('d'.refined) {

  // N.B., This conforms to today's filename but I'm not confident it will
  // remain unchanged.

  /**
   * Dataset filename of the pattern [NS]YYYYMMDDS####.fits.
   */
  case class Filename private (
    site:      Site,
    localDate: LocalDate,
    index:     PosInt
  ) {

    def format: String =
      f"${Filename.parseSiteChar(site)}${Filename.DateFormatter.format(localDate)}S${index.value}%04d.fits"

  }

  object Filename {

    def from(
      site:      Site,
      localDate: LocalDate,
      index:     PosInt
    ): Option[Filename] =
      Option.when(localDate.getYear >= 0 && localDate.getYear <= 9999)(Filename(site, localDate, index))

    private val DateFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("uuuuMMdd")

    private val parseSiteChar: Site => Char = {
      case Site.GN => 'N'
      case Site.GS => 'S'
    }

    given Order[Filename] =
      Order.by(a => (a.site, a.localDate, a.index))

    given Show[Filename] =
      Show.show[Filename](_.format)

    private val FilenamePattern: Regex =
      raw"([NS])(\d{8})S(\d{4}\d*).fits".r

    private val parseSite: String => Option[Site] = {
      case "N" => Site.GN.some
      case "S" => Site.GS.some
      case _   => none
    }

    val parse: String => Option[Filename] = {
      case FilenamePattern(site, date, index) =>
        for {
          s <- parseSite(site)
          d <- allCatch.opt(LocalDate.parse(date, DateFormatter))
          i <- index.toIntOption.flatMap(i => PosInt.from(i).toOption)
          f <- from(s, d, i)
        } yield f

      case _                                  =>
        none
    }

    val FromString: Prism[String, Filename] =
      Prism(parse)(_.format)

  }

}

