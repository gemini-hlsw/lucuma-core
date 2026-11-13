// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.operation

import itac._
import java.nio.file.Path
import cats.effect.ExitCode
import org.apache.poi.ss.usermodel.Workbook
import org.apache.poi.hssf.usermodel.HSSFWorkbookFactory
import cats.effect.Sync
import org.apache.poi.ss.usermodel.FillPatternType
import org.apache.poi.ss.usermodel.IndexedColors
import org.apache.poi.ss.usermodel.CellStyle
import org.apache.poi.ss.usermodel.Cell
import org.apache.poi.ss.usermodel.BorderStyle
import cats.implicits._
import java.nio.file.Paths
import scala.math.BigDecimal.RoundingMode
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import edu.gemini.tac.qengine.api.QueueCalc
import lucuma.core.enums.Site
import edu.gemini.tac.qengine.p1.ProposalShard
import lucuma.core.util.TimeSpan
import edu.gemini.tac.qengine.impl.QueueEngine3
import lucuma.core.data.PerSite
import org.typelevel.log4cats.Logger
import lucuma.core.util.Enumerated

object NgoSpreadsheet {

  // ffs
  trait Excel[A]:
    def setValue(c: Cell, a: A): Unit
  object Excel:
    given Excel[Int] = (c, a) => c.setCellValue(a.toDouble)
    given Excel[BigDecimal] = (c, a) => c.setCellValue(a.setScale(1, RoundingMode.HALF_UP).toDouble)
    given Excel[Double] = (c, a) => summon[Excel[BigDecimal]].setValue(c, a)
    given Excel[String] = (c, a) => c.setCellValue(a)
    given Excel[Boolean] = (c, a) => c.setCellValue(if (a) "TRUE" else "")
    given Excel[TimeSpan] = (c, a) => summon[Excel[BigDecimal]].setValue(c, a.toHours)

  def apply[F[_]: Sync](
    qe:             QueueEngine3.type,
    siteConfig:     PerSite[Path]
  ): Operation[F] =
    new Operation[F] {
      def run(ws: Workspace[F], log: Logger[F]): F[ExitCode] =
        for {
          wbs <- Enumerated[TimeAccountingCategory].all.traverse(p => Sync[F].delay((p, HSSFWorkbookFactory.createWorkbook))) // List[(Partner, Workbook)]
          _   <- addSheets(wbs, qe, siteConfig).run(ws, log)
          cwd <- ws.cwd
          _   <- wbs.traverse { case (p, wb) => Sync[F].delay(wb.write(cwd.resolve(Paths.get(s"itac-results-${p.tag}.xls")).toFile)) }
        } yield ExitCode.Success
    }

  def addSheets[F[_]: Sync](
    wbs:            List[(TimeAccountingCategory, Workbook)],
    qe:             QueueEngine3.type,
    siteConfig:     PerSite[Path],
  ): Operation[F] =
    new AbstractQueueOperation[F](qe, siteConfig) {
      def run(ws: Workspace[F], log: Logger[F]): F[ExitCode] =
        for {
          qc  <- computeQueue(ws).map(_._2)
          _   <- wbs.traverse: (p, wb) => 
                  Enumerated[Site].all.traverse: site =>
                    Sync[F].delay(writeSheet(wb, p, site, qc))
        } yield ExitCode.Success
    }

  val ProgId      =  0 // Gemini Program ID: @PROG_ID@
  val Band        =  1 // Scientific Ranking Band: @QUEUE_BAND@
  val Reference   =  2 // Program xml file number : @NTAC_REF_NUMBER@
  val Rank        =  3 // proposal rank : @rank@
  val PIName      =  4 // Principal Investigator: @PI_NAME@
  val PiEmail     =  5 // Principal Investigator e-mail: @PI_MAIL@
  val Time        =  6 // Time awarded: @TIME_AWARDED@
  val ProgTime    =  7 // Program Time Awarded: @PROG_TIME@
  val PartTime    =  8 // Partner Calibration Time Awarded: @PARTNER_TIME@
  val ItacComment =  9 // ITAC Feedback: @ITAC_COMMENTS@
  val Title       = 10 // Program Title: @PROG_TITLE@

  def writeSheet(wb: Workbook, p: TimeAccountingCategory, site: Site, qr: QueueCalc): Unit = {
    val sh = wb.createSheet(site.longName)

    // A bold font!
    val font = sh.getWorkbook.createFont
    font.setBold(true)

    // A header style!
    val headerStyle = sh.getWorkbook.createCellStyle
    headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND)
    headerStyle.setFillForegroundColor(IndexedColors.GREY_25_PERCENT.getIndex())
    headerStyle.setFont(font)

    // Header
    val h = sh.createRow(0)

    // Helper to create a header cell
    def create(col: Int, value: String, charWidth: Int): Unit = {
      val c = h.createCell(col)
      c.setCellStyle(headerStyle)
      c.setCellValue(value)
      sh.setColumnWidth(col, 256 * charWidth)
    }

    // Create header cells
    create(ProgId, "Gemini Id", 18)
    create(Band, "Band", 5)
    create(Reference, "NGO Reference", 17)
    create(Rank, "Rank", 5)
    create(PIName, "PI Name", 25)
    create(Time, "Time Awarded", 15)
    create(ProgTime, "Program Time", 15)
    create(PartTime, "Partner Cal Time", 15)
    create(PiEmail, "PI Email", 25)
    create(ItacComment, "ITAC Comment", 100)
    create(Title, "Title", 100)

    // A style for each band
    val bandStyles: ScienceBand => CellStyle =
      ScienceBand.values.map { b =>
        val color = b match {
          case ScienceBand.Band1 => IndexedColors.LIGHT_YELLOW.getIndex
          case ScienceBand.Band2 => IndexedColors.LIGHT_GREEN.getIndex
          case ScienceBand.Band3 => IndexedColors.PALE_BLUE.getIndex
          case ScienceBand.Band4 => IndexedColors.GREY_25_PERCENT.getIndex
        }

        val style = sh.getWorkbook.createCellStyle
        style.setFillPattern(FillPatternType.SOLID_FOREGROUND)
        style.setFillForegroundColor(color)
        style.setBorderBottom(BorderStyle.THIN)
        style.setBorderTop(BorderStyle.THIN)
        style.setBorderLeft(BorderStyle.THIN)
        style.setBorderRight(BorderStyle.THIN)
        style.setBottomBorderColor(IndexedColors.GREY_50_PERCENT.getIndex)
        style.setTopBorderColor(IndexedColors.GREY_50_PERCENT.getIndex)
        style.setLeftBorderColor(IndexedColors.GREY_50_PERCENT.getIndex)
        style.setRightBorderColor(IndexedColors.GREY_50_PERCENT.getIndex)
        (b -> style)
      } .toMap

    var n = 1
    ScienceBand.values.foreach { b =>

      // val es: List[QueueCalc.Entry] =
      //   b match {
      //     case ScienceBand.Band1 => qr.classical(ps, p) ++ qr.entries(b, p)
      //     case _                 => qr.entries(b, p)
      //   }

      val ss: List[ProposalShard] =
        qr.queues(site)(b).toList.filter(_.allocation.category === p)

      ss.foreach { s =>

        // val p = e.proposals.head // there will never be more than one because this is per-partner (hm, should change the entry type)
        val p = s.parentProposal
        val r = sh.createRow(n)

        def addCell[A](col: Int, value: A)(implicit ev: Excel[A]): Unit = {
          val c = r.createCell(col)
          c.setCellStyle(bandStyles(b))
          ev.setValue(c, value)
        }

        // val (progTime, partTime) = ProgramPartnerTime.programAndPartnerTime(p, b)

        addCell(Reference, p.reference.label)
        addCell(Rank, p.ranking.value)
        addCell(Band, b.intValue)
        addCell(ProgId, p.programReference.label)
        addCell(PIName, p.piName)
        addCell(PiEmail, "TODO")
        addCell(Time, s.allocation.duration)
        addCell(ProgTime, "TODO") // progTime.toHours.value)
        addCell(PartTime, "TODO") // partTime.toHours.value)
        addCell(ItacComment, "TODO") //p.itacComment.orEmpty)
        addCell(Title, p.title)
        n += 1

      }
    }

    // A style for unsuccessful
    val unsuccessfulStyle:CellStyle = {
      val style = sh.getWorkbook.createCellStyle
      style.setBorderBottom(BorderStyle.THIN)
      style.setBorderTop(BorderStyle.THIN)
      style.setBorderLeft(BorderStyle.THIN)
      style.setBorderRight(BorderStyle.THIN)
      style.setBottomBorderColor(IndexedColors.GREY_50_PERCENT.getIndex)
      style.setTopBorderColor(IndexedColors.GREY_50_PERCENT.getIndex)
      style.setLeftBorderColor(IndexedColors.GREY_50_PERCENT.getIndex)
      style.setRightBorderColor(IndexedColors.GREY_50_PERCENT.getIndex)
      style
    }

    // Unsuccessful
    val ourRejects: List[ProposalShard] =
      qr.rejects.filter: ps =>
        ps.allocation.category === p &&
        ps.site === site

    println(s"rejects for $p = ${ourRejects.map(_.reference)}")

    ourRejects.foreach { ps =>

      val p = ps.parentProposal

      val r = sh.createRow(n)

      def addCell[A](col: Int, value: A)(implicit ev: Excel[A]): Unit = {
        val c = r.createCell(col)
        c.setCellStyle(unsuccessfulStyle)
        ev.setValue(c, value)
      }

      addCell(Reference, p.reference.label)
      addCell(Rank, p.ranking.value)
      addCell(Band, "")
      addCell(ProgId, "")
      addCell(PIName, p.piName)
      addCell(PiEmail, "TODO")
      addCell(Time, 0.0)
      addCell(ProgTime, 0.0)
      addCell(PartTime, 0.0)
      addCell(ItacComment, "TODO")
      addCell(Title, p.title)
      n += 1

    }

  }



}