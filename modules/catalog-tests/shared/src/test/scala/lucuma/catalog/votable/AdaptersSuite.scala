// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.effect.*
import cats.syntax.all.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import eu.timepit.refined.*
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import fs2.*
import fs2.data.xml.*
import lucuma.catalog.*
import lucuma.core.enums.Band
import lucuma.core.enums.CatalogName
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.math.units.*
import lucuma.core.model.CatalogInfo
import lucuma.core.model.Target
import lucuma.refined.*
import munit.CatsEffectSuite

import scala.xml.Utility

class AdaptersSuite extends CatsEffectSuite with VoTableParser with VoTableSamples {

  test("be able to parse a field definition") {
    Stream
      .emits(Utility.trim(gaia).toString)
      .through(events[IO, Char]())
      .through(referenceResolver[IO]())
      .through(normalize[IO])
      .through(VoTableParser.xml2targets[IO](CatalogAdapter.Gaia))
      .compile
      .lastOrError
      .map {
        case Right(CatalogTargetResult(t, _)) =>
          assertEquals(t.name, "Gaia DR2 5500810326779190016".refined[NonEmpty])
          assertEquals(t.tracking.epoch.some, Epoch.Julian.fromEpochYears(2015.5))
          assertEquals(
            t.catalogInfo,
            CatalogInfo(CatalogName.Gaia, "Gaia DR2 5500810326779190016")
          )
          // base coordinates
          assertEquals(
            Target.baseRA.getOption(t),
            RightAscension.fromDoubleDegrees(95.98749097569124).some
          )
          assertEquals(
            Target.baseDec.getOption(t),
            Declination.fromDoubleDegrees(-52.741666247338124)
          )
          // proper motions
          assertEquals(
            Target.properMotionRA.getOption(t),
            ProperMotion.μasyRA(6456).some
          )
          assertEquals(
            Target.properMotionDec.getOption(t),
            ProperMotion.μasyDec(22438).some
          )
          assertEquals(
            Target.integratedBrightnessIn(Band.Gaia).headOption(t),
            BrightnessValue.unsafeFrom(14.292543).withUnit[VegaMagnitude].toMeasureTagged.some
          )
          // parallax
          assertEquals(
            Target.parallax.getOption(t).flatten,
            Parallax.milliarcseconds.reverseGet(3.6810721649521616).some
          )
          // radial velocity
          assertEquals(
            Target.radialVelocity.getOption(t).flatten,
            RadialVelocity(20.30.withUnit[KilometersPerSecond])
          )
        case Left(_)                          =>
          fail("Gaia response could not be parsed")
      }
  }

  test("parse pm corrected fields") {
    Stream
      .emits(Utility.trim(voTableGaiaPMCorrected).toString)
      .through(events[IO, Char]())
      .through(referenceResolver[IO]())
      .through(normalize[IO])
      .through(VoTableParser.xml2guidestars[IO](CatalogAdapter.Gaia))
      .compile
      .lastOrError
      .map {
        case Right(t) =>
          assertEquals(t.name, "Gaia DR2 6050423032358097664".refined[NonEmpty])
          assertEquals(t.tracking.epoch.some, Epoch.Julian.fromEpochYears(2015.5))
          assertEquals(
            t.catalogInfo,
            none
          )
          // base coordinates
          assertEquals(
            Target.baseRA.getOption(t),
            RightAscension.fromDoubleDegrees(244.26317318202356).some
          )
          assertEquals(
            Target.baseDec.getOption(t),
            Declination.fromDoubleDegrees(-22.954945101383874)
          )
          assertEquals(
            Target.integratedBrightnessIn(Band.Gaia).headOption(t),
            BrightnessValue.unsafeFrom(20.755217).withUnit[VegaMagnitude].toMeasureTagged.some
          )
          assertEquals(
            Target.epoch.getOption(t),
            Epoch.fromString.getOption("J2015.500")
          )
        case Left(_)  => fail("Gaia response could not be parsed")
      }
  }
}
