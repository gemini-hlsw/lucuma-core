// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma

import lucuma.core.enums.ScienceBand
import edu.gemini.tac.qengine.p1.Proposal
import lucuma.core.util.Enumerated
import scala.io.Source
import scala.io.Codec
import munit.FunSuite

object Fixture_25B:

  val Root = s"25B"

  extension (b: ScienceBand) def intValue =
    b match
      case ScienceBand.Band1 => 1
      case ScienceBand.Band2 => 2
      case ScienceBand.Band3 => 3
      case ScienceBand.Band4 => 4

  def withSource[A](rsrc: String)(f: Source => A): A =
    val s = Source.fromResource(rsrc)
    try f(s) finally s.close

  def loadProposalIntoBand(band: ScienceBand, yamlFile: String): Proposal =
    println(s"$band -> $yamlFile")
    withSource(yamlFile): s =>
      null

  def loadBand(band: ScienceBand): List[Proposal] =
    withSource(s"$Root/band-${band.intValue}.lst"): s =>
      s.getLines().toList.map: s =>
        loadProposalIntoBand(band, s"$Root/$s.yaml")
    
  def loadAll(): List[Proposal] =
    Enumerated[ScienceBand].all.flatMap(loadBand)

class Fixture_25B extends FunSuite:

  test("x") {
    Fixture_25B.loadAll()
  }