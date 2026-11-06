// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.operation

import itac._
import java.nio.file.Path
import cats.effect.ExitCode
import org.apache.poi.ss.usermodel.Workbook
import cats.implicits._
import org.apache.poi.hssf.usermodel.HSSFWorkbookFactory
import cats.effect.Sync
import org.apache.poi.ss.usermodel.FillPatternType
import org.apache.poi.ss.usermodel.IndexedColors
import org.apache.poi.ss.usermodel.CellStyle
import org.apache.poi.ss.usermodel.Cell
import org.apache.poi.ss.usermodel.BorderStyle
import java.nio.file.Paths
import edu.gemini.tac.qengine.impl.QueueEngine3
import lucuma.core.data.PerSite
import org.typelevel.log4cats.Logger
import lucuma.core.enums.Site
import lucuma.core.enums.ScienceBand
import edu.gemini.tac.qengine.api.queue.ProposalQueue
import lucuma.core.util.Enumerated
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.FacilityObservingModeType
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.enums.TooActivation
import lucuma.core.model.ConstraintSet

object StaffEmailSpreadsheet {

  // ffs
  trait Excel[A] {
    def setValue(c: Cell, a: A): Unit
  }
  object Excel {
    implicit val ExcelInt: Excel[Int] = (c, a) => c.setCellValue(a.toDouble)
    implicit val ExcelDouble: Excel[Double] = (c, a) => c.setCellValue(a)
    implicit val ExcelString: Excel[String] = (c, a) => c.setCellValue(a)
    implicit val ExcelBoolean: Excel[Boolean] = (c, a) => c.setCellValue(if (a) "TRUE" else "")
  }

  def apply[F[_]: Sync](
    qe:             QueueEngine3.type,
    siteConfig:     PerSite[Path]
  ): Operation[F] =
    new AbstractQueueOperation[F](qe, siteConfig) {
      def run(ws: Workspace[F], log: Logger[F]): F[ExitCode] =        
        for {
          qc  <- computeQueue(ws).map(_._2)
          wb  <- Sync[F].delay(HSSFWorkbookFactory.createWorkbook())
          _   <- Enumerated[Site].all.traverse(s => addSheet(wb, s, qc.queues(s)))
          cwd <- ws.cwd
          _   <- Sync[F].delay(wb.write(cwd.resolve(Paths.get("staff-email-spreadsheet.xls")).toFile))
        } yield ExitCode.Success
    }

  val Reference        = 0
  val Band             = 1
  val ProgId           = 2
  val PIName           = 3
  val Time             = 4
  val Instrument       = 5
  val ToO              = 6
  val LGS              = 7
  val NGO              = 8
  val NgoEmail         = 9
  val GeminiEmail      = 10
  val Title            = 11
  val ObsConstraints   = 12
  val RaMin            = 13
  val RaMax            = 14
  val SchedConstraints = 15


  def addSheet[F[_]: Sync](
    wb:    Workbook,
    site:  Site,
    queues: ScienceBand => ProposalQueue,
    // qe:             QueueEngine3.type,
    // siteConfig:     Path,
    // rolloverReport: Option[Path],
  ): F[Unit] = 
    Sync[F].delay {
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
      create(Reference, "Reference", 17)
      create(Band, "Band", 5)
      create(ProgId, "Gemini Id", 18)
      create(PIName, "PI Name", 25)
      create(Time, "Time", 5)
      create(Instrument, "Instrument", 20)
      create(ToO, "ToO", 5)
      create(LGS, "LGS", 6)
      create(NGO, "NGO", 5)
      create(NgoEmail, "NGO/principal support Email", 35)
      create(GeminiEmail, "Gemini/additional support Email", 35)
      create(Title, "Title", 100)
      create(ObsConstraints, "Obs Constraints", 50)
      create(RaMin, "RA Min", 6)
      create(RaMax, "RA Max", 6)
      create(SchedConstraints, "Scheduling", 100)

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
      Enumerated[ScienceBand].all.foreach { b =>

        queues(b).toList.foreach { ps =>

          // for now just take the head
          val p = ps.parentProposal
          val r = sh.createRow(n)

          def addCell[A](col: Int, value: A)(implicit ev: Excel[A]): Unit = {
            val c = r.createCell(col)
            c.setCellStyle(bandStyles(b))
            ev.setValue(c, value)
          }

          // val mpi = m.getInvestigators().getPi()

          addCell(Reference, p.reference.label)
          addCell(Band, b.intValue)
          addCell(ProgId, p.programReference.label)
          addCell(PIName, p.piName)
          addCell(Time, "TODO") // (e.proposals.foldMap(_.time)).toHours.value)

          val insts: List[String] =
            import lucuma.core.enums.Instrument.*
            p.originalObsList.map(_.mode).map: m =>
              m match
                case ExchangeObservingModeType.ExchangeKeck      => "(Keck)"
                case ExchangeObservingModeType.ExchangeSubaru    => "(Subaru)"
                case FacilityObservingModeType.Flamingos2Imaging 
                   | FacilityObservingModeType.Flamingos2LongSlit
                   | FacilityObservingModeType.Flamingos2Mos     => Flamingos2.shortName
                case FacilityObservingModeType.GhostIfu          => Ghost.shortName
                case FacilityObservingModeType.GmosNorthImaging  
                   | FacilityObservingModeType.GmosNorthLongSlit  
                   | FacilityObservingModeType.GmosNorthIfu      
                   | FacilityObservingModeType.GmosNorthMos      => GmosNorth.shortName
                case FacilityObservingModeType.GmosSouthImaging  
                   | FacilityObservingModeType.GmosSouthLongSlit 
                   | FacilityObservingModeType.GmosSouthIfu      
                   | FacilityObservingModeType.GmosSouthMos      => GmosSouth.shortName
                case FacilityObservingModeType.GnirsImaging      
                   | FacilityObservingModeType.GnirsLongSlit     
                   | FacilityObservingModeType.GnirsIfu          => Gnirs.shortName
                case FacilityObservingModeType.Igrins2LongSlit   => Igrins2.shortName
                case VisitorObservingModeType.AlopekeSpeckle     
                   | VisitorObservingModeType.AlopekeWideField   => Alopeke.shortName
                case VisitorObservingModeType.VisitorNorth        
                   | VisitorObservingModeType.VisitorSouth       => "(Visitor)"
                case VisitorObservingModeType.MaroonX            => MaroonX.shortName
                case VisitorObservingModeType.ZorroSpeckle       
                   | VisitorObservingModeType.ZorroWideField     => Zorro.shortName

          addCell(Instrument, insts.mkString(", "))

          addCell(ToO, 
            p.too match
              case TooActivation.None         => ""
              case TooActivation.Standard     => "S"
              case TooActivation.Rapid        => "R"
              case TooActivation.Interrupting => "I"
          )
          
          def format(cs: ConstraintSet): String =
            s"${cs.imageQuality.tag}/${cs.cloudExtinction.tag}/${cs.skyBackground.label}/${cs.waterVapor.label}"

          addCell(LGS, "TODO")
          addCell(NGO, ps.allocation.category.abbreviation)
          addCell(NgoEmail, "TODO") // ngoInfo.flatMap(_.ngoEmail).orEmpty)
          addCell(GeminiEmail, "")
          addCell(Title, p.title)
          addCell(ObsConstraints, ps.observations.map(_.constraintSet).map(format).distinct.mkString(", "))

          val ras = ps.observations.map(_.itacTarget.coords.ra.toHourAngle.toDoubleHours)
          ras.minimumOption.fold(addCell(RaMin, ""))(addCell(RaMin, _))
          ras.maximumOption.fold(addCell(RaMax, ""))(addCell(RaMax, _))

          addCell(SchedConstraints, "TODO")

          n += 1
        }
      }

    }


}