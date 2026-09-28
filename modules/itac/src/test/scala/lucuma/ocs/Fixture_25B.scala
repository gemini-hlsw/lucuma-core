// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ocs

import edu.gemini.tac.qengine.p1.Proposal
import lucuma.core.enums.ScienceBand
import munit.FunSuite
import cats.syntax.all.*

import scala.io.Codec
import scala.io.Source
import java.io.File
import scala.xml.XML

object Fixture_25B:

  val Root = s"25B"

  extension (b: ScienceBand) def intValue =
    b match
      case ScienceBand.Band1 => 1
      case ScienceBand.Band2 => 2
      case ScienceBand.Band3 => 3
      case ScienceBand.Band4 => 4

  def withResource[A](rsrc: String)(f: Source => A): A =
    val s = Source.fromResource(rsrc)
    try f(s) finally s.close
  

  def loadProposal(band: ScienceBand, file: File): Either[String, Proposal] =
    val root = Anonymizer.anonymize(XML.load(file))
    Converter.convert(root, band)

  def loadBand(band: ScienceBand): Either[String, List[Proposal]] =
    val dir = new File(s"/Users/rob.norris/Gemini/ocs/itac/itac_WD/band-${band.intValue}/")
    dir
      .listFiles
      .toList
      .filter(_.getName().endsWith(".xml"))
      .traverse(loadProposal(band, _))

  def loadAll() =
    loadBand(ScienceBand.Band1)

class Fixture_25B extends FunSuite:

  test("x") {
    println(Fixture_25B.loadAll())
  }

