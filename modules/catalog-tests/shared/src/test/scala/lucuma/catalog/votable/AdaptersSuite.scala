// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import algebra.instances.all.*
import cats.effect.*
import cats.syntax.all.*
import coulomb.*
import coulomb.conversion.implicits.given
import coulomb.syntax.*
import eu.timepit.refined.collection.NonEmpty
import fs2.*
import fs2.data.xml.*
import lucuma.catalog.*
import lucuma.core.enums.Band
import lucuma.core.enums.CatalogName
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.math.syntax.int.*
import lucuma.core.math.units.*
import lucuma.core.model.CatalogInfo
import lucuma.core.model.Target
import lucuma.core.refined.auto.*
import munit.CatsEffectSuite

import scala.language.implicitConversions
import scala.xml.Utility

class AdaptersSuite extends CatsEffectSuite with VoTableParser with VoTableSamples {

  test("be able to parse a field definition") {
    Stream
      .emits(Utility.trim(gaia).toString)
      .through(events[IO, Char]())
      .through(referenceResolver[IO]())
      .through(normalize[IO])
      .through(VoTableParser.xml2targets[IO](CatalogAdapter.Gaia3Esa))
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
            RadialVelocity(BigDecimal(20.30).withUnit[KilometersPerSecond])
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
      .through(VoTableParser.xml2guidestars[IO](CatalogAdapter.Gaia3Esa))
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

  test("parse alternative votable (from GAVO (dc.g-vo.org))") {
    Stream
      .emit[IO, String](Utility.trim(voTableAlternative).toString)
      .through(CatalogSearch.guideStars[IO](CatalogAdapter.Gaia3LiteGavo))
      .compile
      .lastOrError
      .map {
        case Right(t) =>
          assertEquals(t.name, "Gaia DR3 5717266473658972672".refined[NonEmpty])
          assertEquals(t.tracking.epoch.some, Epoch.Julian.fromEpochYears(2016.0))
          assertEquals(t.catalogInfo, none)
          // base coordinates
          assertEquals(
            Target.baseRA.getOption(t),
            RightAscension.fromDoubleDegrees(115.1270374417545).some
          )
          assertEquals(
            Target.baseDec.getOption(t),
            Declination.fromDoubleDegrees(-17.479177473210687)
          )
          // proper motions
          assertEquals(
            Target.properMotionRA.getOption(t),
            ProperMotion.RA.milliarcsecondsPerYear.reverseGet(0.49613327).some
          )
          assertEquals(
            Target.properMotionDec.getOption(t),
            ProperMotion.Dec.milliarcsecondsPerYear.reverseGet(-8.2364235).some
          )
        case Left(e)  =>
          fail(s"Gaia response could not be parsed: ${e.toList.mkString("; ")}")
      }
  }

  test("guide stars keep every Gaia band so R can be estimated") {
    Stream
      .emits(Utility.trim(voTableGaiaGuideStar).toString)
      .through(events[IO, Char]())
      .through(referenceResolver[IO]())
      .through(normalize[IO])
      .through(VoTableParser.xml2guidestars[IO](CatalogAdapter.Gaia3Esa))
      .compile
      .lastOrError
      .map {
        case Right(t) =>
          assertEquals(
            Target.integratedBrightnessIn(Band.Gaia).headOption(t).map(_.value),
            BrightnessValue.unsafeFrom(14.5).some
          )
          assertEquals(
            Target.integratedBrightnessIn(Band.GaiaBP).headOption(t).map(_.value),
            BrightnessValue.unsafeFrom(15.2).some
          )
          assertEquals(
            Target.integratedBrightnessIn(Band.GaiaRP).headOption(t).map(_.value),
            BrightnessValue.unsafeFrom(13.7).some
          )
        case Left(e)  => fail(s"Gaia response could not be parsed: ${e.toList.mkString("; ")}")
      }
  }

  test("an R band constraint queries G widened by the G - R range") {
    given ADQLInterpreter = ADQLInterpreter.nTarget(10)
    val constraints       = BrightnessConstraints(
      BandsList.RBandsList,
      FaintnessConstraint(BrightnessValue.unsafeFrom(15.0)),
      SaturationConstraint(BrightnessValue.unsafeFrom(8.0)).some
    )
    val query             = QueryByADQL(
      Coordinates.Zero,
      ShapeExpression.centeredEllipse(10.arcseconds, 10.arcseconds),
      constraints.some
    )
    val adql              = summon[ADQLInterpreter].buildQueryString(CatalogAdapter.Gaia3Esa, query)
    assert(
      adql.contains(
        "((phot_g_mean_mag between 7.640 and 15.270) and phot_bp_mean_mag - phot_rp_mean_mag between 0.0 and 4.0)"
      ),
      adql
    )
  }

  test("a Gaia band constraint queries the Gaia columns unchanged") {
    given ADQLInterpreter = ADQLInterpreter.nTarget(10)
    val constraints       = BrightnessConstraints(
      BandsList.GaiaBandsList,
      FaintnessConstraint(BrightnessValue.unsafeFrom(15.0)),
      none
    )
    val query             = QueryByADQL(
      Coordinates.Zero,
      ShapeExpression.centeredEllipse(10.arcseconds, 10.arcseconds),
      constraints.some
    )
    val adql              = summon[ADQLInterpreter].buildQueryString(CatalogAdapter.Gaia3Esa, query)
    assert(
      adql.contains(
        "(phot_rp_mean_mag < 15.000) or (phot_g_mean_mag < 15.000) or (phot_bp_mean_mag < 15.000)"
      ),
      adql
    )
  }
}
