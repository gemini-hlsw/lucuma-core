// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.effect.*
import cats.implicits.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import eu.timepit.refined.*
import eu.timepit.refined.collection.NonEmpty
import fs2.*
import fs2.io.file.Files
import fs2.io.file.Path
import lucuma.catalog.*
import lucuma.core.enums.Band
import lucuma.core.enums.CatalogName
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
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

class ParseSimbadFileSuite extends CatsEffectSuite with VoTableParser {

  test("parse simbad named queries") {
    // From http://simbad.u-strasbg.fr/simbad/sim-id?Ident=Vega&output.format=VOTable
    val xmlFile = "/simbad-vega.xml"
    // The sample has only one row
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(CatalogSearch.siderealTargets(CatalogAdapter.Simbad))
        .compile
        .lastOrError
        .map {
          case Right(CatalogTargetResult(t, _)) =>
            // id and search name
            assertEquals(t.name, "Vega".refined[NonEmpty])
            assertEquals(
              t.catalogInfo,
              CatalogInfo(CatalogName.Simbad, "* alf Lyr", "PulsV*delSct, A0Va")
            )
            // base coordinates
            assertEquals(
              Target.baseRA.getOption(t),
              RightAscension.fromDoubleDegrees(279.23473479).some
            )
            assertEquals(
              Target.baseDec.getOption(t),
              Declination.fromDoubleDegrees(38.78368896)
            )
            // proper motions
            assertEquals(
              Target.properMotionRA.getOption(t),
              ProperMotion.μasyRA(200940).some
            )
            assertEquals(
              Target.properMotionDec.getOption(t),
              ProperMotion.μasyDec(286230).some
            )
            // brightnesses
            assertEquals(
              Target.integratedBrightnessIn(Band.U).headOption(t),
              BrightnessValue.unsafeFrom(0.03).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.B).headOption(t),
              BrightnessValue.unsafeFrom(0.03).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.V).headOption(t),
              BrightnessValue.unsafeFrom(0.03).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.R).headOption(t),
              BrightnessValue.unsafeFrom(0.07).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.I).headOption(t),
              BrightnessValue.unsafeFrom(0.10).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.J).headOption(t),
              BrightnessValue.unsafeFrom(-0.18).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.H).headOption(t),
              BrightnessValue.unsafeFrom(-0.03).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.K).headOption(t),
              BrightnessValue.unsafeFrom(0.13).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            // parallax
            assertEquals(
              Target.parallax.getOption(t).flatten,
              Parallax.milliarcseconds.reverseGet(130.23).some
            )
            // radial velocity
            assertEquals(
              Target.radialVelocity.getOption(t).flatten,
              RadialVelocity(-20.60.withUnit[KilometersPerSecond])
            )
          case Left(_)                          => fail(s"VOTable xml $xmlFile cannot be parsed")
        }
    }
  }

  test("parse simbad named queries with sloan band brightnesses") {
    // From http://simbad.u-strasbg.fr/simbad/sim-id?Ident=2MFGC6625&output.format=VOTable
    val xmlFile = "/simbad-2MFGC6625.xml"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(CatalogSearch.siderealTargets(CatalogAdapter.Simbad))
        .compile
        .lastOrError
        .map {
          case Right(CatalogTargetResult(t, angularSize)) =>
            // id and search name
            assertEquals(t.name, "2MFGC6625".refined[NonEmpty])
            assertEquals(t.catalogInfo, CatalogInfo(CatalogName.Simbad, "2MFGC 6625", "EmG, I"))
            // base coordinates
            assertEquals(
              Target.baseRA.getOption(t),
              RightAscension.fromHourAngle
                .get(HourAngle.fromHMS(8, 23, 54, 966, 933))
                .some
            )
            assertEquals(
              Target.baseDec.getOption(t),
              Declination
                .fromAngleWithCarry(Angle.fromDMS(28, 6, 21, 605, 409))
                ._1
                .some
            )
            // proper velocity
            assertEquals(Target.Sidereal.properMotion.get(t), none)
            // radial velocity
            assertEquals(
              Target.radialVelocity.getOption(t).flatten,
              RadialVelocity(13822.withUnit[KilometersPerSecond])
            )
            // parallax
            assertEquals(
              Target.parallax.getOption(t).flatten,
              none
            )
            // band brightnesses
            assertEquals(
              Target.integratedBrightnessIn(Band.SloanU).headOption(t),
              BrightnessValue
                .unsafeFrom(16.284)
                .withUnit[ABMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.007))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.SloanG).headOption(t),
              BrightnessValue
                .unsafeFrom(15.728)
                .withUnit[ABMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.003))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.SloanR).headOption(t),
              BrightnessValue
                .unsafeFrom(15.986)
                .withUnit[ABMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.004))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.SloanI).headOption(t),
              BrightnessValue
                .unsafeFrom(15.603)
                .withUnit[ABMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.004))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.SloanZ).headOption(t),
              BrightnessValue
                .unsafeFrom(15.682)
                .withUnit[ABMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.008))
                .some
            )
            // angular size
            assertEquals(
              angularSize,
              AngularSize(Angle.fromDMS(0, 0, 35, 400, 0), Angle.fromDMS(0, 0, 6, 359, 999)).some
            )
          case Left(_)                                    => fail(s"VOTable xml $xmlFile cannot be parsed")
        }
    }
  }
  test("parse simbad named queries with mixed band brightnesses") {
    // From http://simbad.u-strasbg.fr/simbad/sim-id?Ident=2SLAQ%20J000008.13%2B001634.6&output.format=VOTable
    val xmlFile = "/simbad-J000008.13.xml"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(CatalogSearch.siderealTargets(CatalogAdapter.Simbad))
        .compile
        .lastOrError
        .map {
          case Right(CatalogTargetResult(t, _)) =>
            // id and search name
            assertEquals(t.name, "2SLAQ J000008.13+001634.6".refined[NonEmpty])
            assertEquals(
              t.catalogInfo,
              CatalogInfo(CatalogName.Simbad, "2SLAQ J000008.13+001634.6", "QSO")
            )
            // base coordinates
            assertEquals(
              Target.baseRA.getOption(t),
              RightAscension.fromHourAngle
                .get(HourAngle.fromHMS(0, 0, 8, 135, 999))
                .some
            )
            assertEquals(
              Target.baseDec.getOption(t),
              Declination
                .fromAngleWithCarry(Angle.fromDMS(0, 16, 34, 690, 799))
                ._1
                .some
            )
            // proper velocity
            assertEquals(Target.Sidereal.properMotion.get(t), none)
            // radial velocity
            assertEquals(
              Target.radialVelocity.getOption(t).flatten,
              RadialVelocity(233509.withUnit[KilometersPerSecond])
            )
            // parallax
            assertEquals(Target.parallax.getOption(t).flatten, none)
            // band brightnesses
            assertEquals(
              Target.integratedBrightnessIn(Band.B).headOption(t),
              BrightnessValue.unsafeFrom(20.35).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.V).headOption(t),
              BrightnessValue.unsafeFrom(20.03).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.V).headOption(t),
              BrightnessValue.unsafeFrom(20.03).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            // Bands J, H and K for this target have no standard brightness units
            assertEquals(
              Target.integratedBrightnessIn(Band.J).headOption(t),
              BrightnessValue
                .unsafeFrom(19.399)
                .withUnit[ABMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.073))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.H).headOption(t),
              BrightnessValue
                .unsafeFrom(19.416)
                .withUnit[ABMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.137))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.K).headOption(t),
              BrightnessValue
                .unsafeFrom(19.176)
                .withUnit[ABMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.115))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.SloanU).headOption(t),
              BrightnessValue
                .unsafeFrom(20.233)
                .withUnit[ABMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.054))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.SloanG).headOption(t),
              BrightnessValue
                .unsafeFrom(20.201)
                .withUnit[ABMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.021))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.SloanR).headOption(t),
              BrightnessValue
                .unsafeFrom(19.929)
                .withUnit[ABMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.021))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.SloanI).headOption(t),
              BrightnessValue
                .unsafeFrom(19.472)
                .withUnit[ABMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.023))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.SloanZ).headOption(t),
              BrightnessValue
                .unsafeFrom(19.191)
                .withUnit[ABMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.068))
                .some
            )
          case Left(_)                          => fail(s"VOTable xml $xmlFile cannot be parsed")
        }
    }
  }
  test("force negative parallax values to zero") {
    // From http://simbad.u-strasbg.fr/simbad/sim-id?output.format=VOTable&Ident=HIP43018
    val xmlFile = "/simbad_hip43018.xml"

    val file = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(CatalogSearch.siderealTargets(CatalogAdapter.Simbad))
        .compile
        .lastOrError
        .map {
          case Right(CatalogTargetResult(t, _)) =>
            // parallax
            assertEquals(
              Target.parallax.getOption(t).flatten,
              Parallax.Zero.some
            )
          case Left(_)                          => fail(s"VOTable xml $xmlFile cannot be parsed")
        }
    }
  }

  test("parse simbad with a not-found name") {
    val xmlFile = "/simbad-not-found.xml"
    // Simbad returns non-valid xml when an element is not found, we need to skip validation :S
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(CatalogSearch.siderealTargets(CatalogAdapter.Simbad))
        .compile
        .last
        .map {
          case Some(_) => fail("Cannot parse values")
          case _       => assert(true)
        }
    }
  }
  test("parse simbad with an npe") {
    val xmlFile = "/simbad-npe.xml"
    // Simbad returns non-valid xml when there is an internal error like an NPE
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(CatalogSearch.siderealTargets(CatalogAdapter.Simbad))
        .compile
        .last
        .map {
          case Some(_) => fail("Cannot parse values")
          case _       => assert(true)
        }
    }
  }

  test("support simbad repeated band brightnesses entries and angular size") {
    val xmlFile = "/simbad-ngc-2438.xml"
    // Simbad returns an xml with multiple measurements of the same band, use only the first one
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(CatalogSearch.siderealTargets(CatalogAdapter.Simbad))
        .compile
        .lastOrError
        .map {
          case Right(CatalogTargetResult(t, angularSize)) =>
            // id and search name
            assertEquals(t.name, "NGC 2438".refined[NonEmpty])
            assertEquals(t.catalogInfo, CatalogInfo(CatalogName.Simbad, "NGC  2438", "PN"))
            assert(t.tracking.properMotion.isEmpty)
            assertEquals(
              Target.integratedBrightnessIn(Band.J).headOption(t),
              BrightnessValue
                .unsafeFrom(17.02)
                .withUnit[VegaMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.15))
                .some
            )
            assertEquals(
              angularSize,
              AngularSize(Angle.fromDMS(0, 1, 10, 380, 0), Angle.fromDMS(0, 1, 10, 380, 0)).some
            )
          case Left(_)                                    => fail(s"VOTable xml $xmlFile cannot be parsed")
        }
    }
  }

  test("parse xml with missing pm component") {
    // Taken from the url below and manually edited to remove PM RA
    // From http://simbad.u-strasbg.fr/simbad/sim-id?Ident=Vega&output.format=VOTable
    val xmlFile = "/simbad-vega-partial-pm.xml"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(CatalogSearch.siderealTargets(CatalogAdapter.Simbad))
        .compile
        .lastOrError
        .map {
          case Right(CatalogTargetResult(t, _)) =>
            // proper motions
            assertEquals(
              Target.properMotionRA.getOption(t),
              ProperMotion.ZeroRAVelocity.some
            )
            assertEquals(
              Target.properMotionDec.getOption(t),
              ProperMotion.μasyDec(286230).some
            )
          case Left(_)                          => fail(s"VOTable xml $xmlFile cannot be parsed")
        }
    }
  }

  // https://app.shortcut.com/lucuma/story/3696/target-catalog-search-not-working-seems-to-be-a-change-in-simbad
  // Simbad changed the way they were returning results from the strasbg url, but, at the time at least, the harvard
  // mirror returned results in the old format. A change was made to handle either result.
  // This test tests the old strasbg results (same as harvard).
  // The next test tests the new results.
  test("parse simbad wildcard queries with name") {
    // From https://simbad.u-strasbg.fr/simbad/sim-id?Ident=name+vega*&NbIdent=wild&output.format=VOTable
    val xmlFile = "/simbad-vega-name.xml"
    // The sample has only one row
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(CatalogSearch.siderealTargets(CatalogAdapter.Simbad))
        .compile
        .lastOrError
        .map {
          case Right(CatalogTargetResult(t, _)) =>
            // id and search name
            assertEquals(t.name, "Vega".refined[NonEmpty])
            assertEquals(
              t.catalogInfo,
              CatalogInfo(CatalogName.Simbad, "* alf Lyr", "PulsV*delSct, A0Va")
            )
            // base coordinates
            assertEquals(
              Target.baseRA.getOption(t),
              RightAscension.fromDoubleDegrees(279.23473479).some
            )
            assertEquals(
              Target.baseDec.getOption(t),
              Declination.fromDoubleDegrees(38.78368896)
            )
            // proper motions
            assertEquals(
              Target.properMotionRA.getOption(t),
              ProperMotion.μasyRA(200940).some
            )
            assertEquals(
              Target.properMotionDec.getOption(t),
              ProperMotion.μasyDec(286230).some
            )
            // band brightnesses
            assertEquals(
              Target.integratedBrightnessIn(Band.U).headOption(t),
              BrightnessValue.unsafeFrom(0.03).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.B).headOption(t),
              BrightnessValue.unsafeFrom(0.03).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.V).headOption(t),
              BrightnessValue.unsafeFrom(0.03).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.R).headOption(t),
              BrightnessValue.unsafeFrom(0.07).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.I).headOption(t),
              BrightnessValue.unsafeFrom(0.10).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.J).headOption(t),
              BrightnessValue
                .unsafeFrom(-0.177)
                .withUnit[VegaMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.206))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.H).headOption(t),
              BrightnessValue
                .unsafeFrom(-0.029)
                .withUnit[VegaMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.146))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.K).headOption(t),
              BrightnessValue
                .unsafeFrom(0.129)
                .withUnit[VegaMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.186))
                .some
            )
            // parallax
            assertEquals(
              Target.parallax.getOption(t).flatten,
              Parallax.milliarcseconds.reverseGet(130.23).some
            )
            // radial velocity
            assertEquals(
              Target.radialVelocity.getOption(t).flatten,
              RadialVelocity(-20.60.withUnit[KilometersPerSecond])
            )
          case Left(_)                          => fail(s"VOTable xml $xmlFile cannot be parsed")
        }
    }
  }

  // See comment for previous test
  test("sc-3696 parse simbad wildcard queries with name 2") {
    // From the old format https://simbad.u-strasbg.fr/simbad/sim-id?Ident=name+vega*&NbIdent=wild&output.format=VOTable
    val xmlFile = "/simbad-vega-name2.xml"
    // The sample has only one row
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(CatalogSearch.siderealTargets(CatalogAdapter.Simbad))
        .compile
        .lastOrError
        .map {
          case Right(CatalogTargetResult(t, _)) =>
            // id and search name
            assertEquals(t.name, "Vega".refined[NonEmpty])
            assertEquals(
              t.catalogInfo,
              CatalogInfo(CatalogName.Simbad, "* alf Lyr", "PulsV*delSct, A0Va")
            )
            // base coordinates
            assertEquals(
              Target.baseRA.getOption(t),
              RightAscension.fromDoubleDegrees(279.23473479).some
            )
            assertEquals(
              Target.baseDec.getOption(t),
              Declination.fromDoubleDegrees(38.78368896)
            )
            // proper motions
            assertEquals(
              Target.properMotionRA.getOption(t),
              ProperMotion.μasyRA(200940).some
            )
            assertEquals(
              Target.properMotionDec.getOption(t),
              ProperMotion.μasyDec(286230).some
            )
            // band brightnesses
            assertEquals(
              Target.integratedBrightnessIn(Band.U).headOption(t),
              BrightnessValue.unsafeFrom(0.03).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.B).headOption(t),
              BrightnessValue.unsafeFrom(0.03).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.V).headOption(t),
              BrightnessValue.unsafeFrom(0.03).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.R).headOption(t),
              BrightnessValue.unsafeFrom(0.07).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.I).headOption(t),
              BrightnessValue.unsafeFrom(0.10).withUnit[VegaMagnitude].toMeasureTagged.some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.J).headOption(t),
              BrightnessValue
                .unsafeFrom(-0.177)
                .withUnit[VegaMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.206))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.H).headOption(t),
              BrightnessValue
                .unsafeFrom(-0.029)
                .withUnit[VegaMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.146))
                .some
            )
            assertEquals(
              Target.integratedBrightnessIn(Band.K).headOption(t),
              BrightnessValue
                .unsafeFrom(0.129)
                .withUnit[VegaMagnitude]
                .toMeasureTagged
                .withError(BrightnessValue.unsafeFrom(0.186))
                .some
            )
            // parallax
            assertEquals(
              Target.parallax.getOption(t).flatten,
              Parallax.milliarcseconds.reverseGet(130.23).some
            )
            // radial velocity
            assertEquals(
              Target.radialVelocity.getOption(t).flatten,
              RadialVelocity(-20.60.withUnit[KilometersPerSecond])
            )
          case Left(_)                          => fail(s"VOTable xml $xmlFile cannot be parsed")
        }
    }
  }
}
