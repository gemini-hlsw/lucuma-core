// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.p1.ImageQuality.IQ20
import edu.gemini.tac.qengine.p1.{Mode, Too, QueueBand, Proposal}
import edu.gemini.tac.qengine.p1.QueueBand._

case class BandRestriction(name: String, bands: Set[QueueBand])(val matches: Proposal => Boolean)

object BandRestriction {
  def name(base: String): String = base // "%s Band Restriction".format(base)

  val LpName       = name("LP")
  val Iq20Name     = name("IQ20")
  val LgsName      = name("LGS")
  val RapidTooName = name("Rapid TOO")
  val NotBand3Name = name("Not B3 Amenable")

  def largeProgram: BandRestriction =
    BandRestriction(LpName, Set(QBand1, QBand2)) { _.mode == Mode.LargeProgram }

  def rapidToo: BandRestriction = rapidToo(QBand1)

  def rapidToo(band: QueueBand*) =
    BandRestriction(RapidTooName, Set(band*)) { _.too == Too.rapid }

  def lgs: BandRestriction =
    BandRestriction(LgsName, Set(QBand1, QBand2)) { prop =>
      prop.band3Observations.exists(_.lgs)
    }

  // TODO:
  // This will be an issue if we're going to have to ignore the
  // band3 setting.  In that case, BandRestriction needs a band argument and
  // we need to search the obsList for any obs that has an IQ20 value

  def iq20: BandRestriction =
    BandRestriction(Iq20Name, Set(QBand1, QBand2)) {
      prop => prop.band3Observations.exists(_.conditions.iq == IQ20)
    }

  // Required to remove any proposal that is not-band3 that is pushed into
  // band 3 by a joint component moving up in the queue.
  def notBand3: BandRestriction =
      BandRestriction(NotBand3Name, Set(QBand1, QBand2)) { _.band3Observations.isEmpty }
}