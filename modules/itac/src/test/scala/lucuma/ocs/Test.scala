// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ocs

import java.io.File
import java.io.FileReader
import scala.xml.XML
import scala.xml.Elem
import lucuma.core.enums.ScienceBand
import cats.arrow.FunctionK
import edu.gemini.tac.qengine.p1.ItacGroup

object Test:

  @main def main: Unit =
    val dir = new File("/Users/rob.norris/Gemini/ocs/itac/itac_WD/band-1/")
    dir
      .listFiles
      .toList
      .filter(_.getName().endsWith(".xml"))
      .foreach: file =>
        println(file)
        val elem = XML.load(file)
        println("\n\n\n")
        val out = Converter.convert(Anonymizer.anonymize(elem), ScienceBand.Band1)
        println(out)
        if (out.isLeft) then sys.exit()
        out.foreach: pr=>
          pr.groupTree
            .foldMap:
              new FunctionK[ItacGroup, List]:
                def apply[A](a: ItacGroup[A]) = a.children
            .foreach(println)
                          
  def load(f: File): Elem =
    val r = new FileReader(f)
    try XML.load(r) finally r.close()
