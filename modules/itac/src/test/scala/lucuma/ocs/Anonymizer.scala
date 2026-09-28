// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ocs

import scala.xml.Elem
import munit.internal.io.PlatformIO.File
import scala.xml.XML

object Anonymizer:

  def anonymize(root: Elem): Elem =
    <proposal>
      { root \ "semester" }
      { root \ "targets" }
      { root \ "conditions" }
      { root \ "blueprints" }
      { root \ "observations" }
      { root \ "proposalClass" }
    </proposal>

  def main: Unit =
    val dir = new File("/Users/rob.norris/Gemini/ocs/itac/itac_WD/band-1/")
    dir
      .listFiles
      .toList
      .filter(_.getName().endsWith(".xml"))
      .foreach: file =>
        val root = anonymize(XML.load(file))
        println((root \ "proposalClass" \\ "receipt" \ "id").text)
