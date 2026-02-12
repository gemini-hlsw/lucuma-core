// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.p1.ImageQuality.IQ20
import edu.gemini.tac.qengine.p1.Proposal
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.ToOActivation

import ScienceBand.*

case class BandRestriction(name: String, bands: Set[ScienceBand])(val matches: Proposal => Boolean)

object BandRestriction {
  def name(base: String): String = base // "%s Band Restriction".format(base)

  val LpName       = name("LP")
  val Iq20Name     = name("IQ20")
  val LgsName      = name("LGS")
  val RapidTooName = name("Rapid TOO")
  val NotBand3Name = name("Not B3 Amenable")

  def largeProgram: BandRestriction =
    BandRestriction(LpName, Set(Band1, Band2)) { _.mode == ScienceSubtype.LargeProgram }

  def rapidToo: BandRestriction = rapidToo(Band1)

  def rapidToo(band: ScienceBand*) =
    BandRestriction(RapidTooName, Set(band*)) { _.too == ToOActivation.Rapid }

  def lgs: BandRestriction =
    BandRestriction(LgsName, Set(Band1, Band2)) { prop =>
      prop.band3Observations.exists(_.lgs)
    }

  // TODO:
  // This will be an issue if we're going to have to ignore the
  // band3 setting.  In that case, BandRestriction needs a band argument and
  // we need to search the obsList for any obs that has an IQ20 value

  def iq20: BandRestriction =
    BandRestriction(Iq20Name, Set(Band1, Band2)) {
      prop => prop.band3Observations.exists(_.conditions.iq == IQ20)
    }

  // Required to remove any proposal that is not-band3 that is pushed into
  // band 3 by a joint component moving up in the queue.
  def notBand3: BandRestriction =
      BandRestriction(NotBand3Name, Set(Band1, Band2)) { _.band3Observations.isEmpty }
}