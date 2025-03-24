// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.csv

import cats.effect.*
import cats.syntax.all.*
import fs2.*
import fs2.io.file.Files
import fs2.io.file.Path
import lucuma.catalog.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.RadialVelocity
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import munit.CatsEffectSuite
import org.http4s.jdkhttpclient.JdkHttpClient

class TargetImportFileSuite extends CatsEffectSuite:

  test("parallax-rv-v") {
    val xmlFile = "/targets_pv.csv"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(TargetImport.csv2targets)
        .compile
        .toList
        // .flatTap(x => IO(pprint.pprintln(x)))
        .map { l =>
          assertEquals(l.length, 4)
          // parallax
          assertEquals(
            l.count {
              case Right(Target.Sidereal(_, s: SiderealTracking, _, _)) =>
                s.parallax.exists(_ === Parallax.fromMicroarcseconds(1000))
              case _                                                    => false
            },
            1
          )
          // plain rv
          assertEquals(
            l.count {
              case Right(Target.Sidereal(_, s: SiderealTracking, _, _)) =>
                s.radialVelocity === RadialVelocity.kilometerspersecond.getOption(1)
              case _                                                    => false
            },
            1
          )
          // z converted to rv
          assertEquals(
            l.count {
              case Right(Target.Sidereal(_, s: SiderealTracking, _, _)) =>
                s.radialVelocity === RadialVelocity.fromMetersPerSecond.getOption(179875474.8)
              case _                                                    => false
            },
            1
          )
          // rv overrides z
          assertEquals(
            l.count {
              case Right(Target.Sidereal(_, s: SiderealTracking, _, _)) =>
                s.radialVelocity === RadialVelocity.kilometerspersecond.getOption(2)
              case _                                                    => false
            },
            1
          )
        }
    }
  }

  test("Another test case") {
    val xmlFile = "/another_test.csv"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(TargetImport.csv2targets)
        .compile
        .toList
        // .flatTap(x => IO(pprint.pprintln(x)))
        .map { l =>
          assertEquals(l.length, 2)
          assertEquals(
            l.count {
              case Right(Target.Sidereal(_, s: SiderealTracking, _, _))
                  if s.epoch.epochYear === 2015.5 =>
                true
              case _ => false
            },
            1
          )
        }
    }
  }

  test("parse sample pit file") {
    val xmlFile = "/PIT_sidereal.csv"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(TargetImport.csv2targets)
        .compile
        .toList
        // .flatTap(x => IO(pprint.pprintln(x)))
        .map { l =>
          assertEquals(l.length, 7)
          assertEquals(l.count {
                         case Right(Target.Sidereal(_, _, SourceProfile.Uniform(_), _)) => true
                         case _                                                         => false
                       },
                       2
          )
        }
    }
  }

  test("parse file with no brightness units") {
    val xmlFile = "/assign_units_sidereal.csv"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(TargetImport.csv2targets)
        .compile
        .toList
        // .flatTap(x => IO(pprint.pprintln(x)))
        .map { l =>
          assertEquals(l.length, 2)
          assertEquals(
            l.count {
              case Right(
                    Target.Sidereal(_,
                                    _,
                                    SourceProfile.Point(SpectralDefinition.BandNormalized(_, m)),
                                    _
                    )
                  ) if (m.count { case ((_, m)) =>
                    m.units.serialized === "VEGA_MAGNITUDE"
                  }) === 1 =>
                true
              case _ => false
            },
            1
          )
          assertEquals(
            l.count {
              case Right(
                    Target.Sidereal(_,
                                    _,
                                    SourceProfile.Point(SpectralDefinition.BandNormalized(_, m)),
                                    _
                    )
                  ) if (m.count { case ((_, m)) =>
                    m.units.serialized === "AB_MAGNITUDE"
                  }) === 1 =>
                true
              case _ => false
            },
            1
          )
        }
    }
  }

  test("parse sample pit file with mixed units") {
    val xmlFile = "/PIT_sidereal_mixed_units.csv"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(TargetImport.csv2targets)
        .compile
        .toList
        // .flatTap(x => IO(pprint.pprintln(x)))
        .map { l =>
          assertEquals(l.length, 1)
          assertEquals(l.count {
                         case Left(_) => true
                         case _       => false
                       },
                       1
          )
        }
    }
  }

  test("parse stars sample file") {
    val xmlFile = "/stars.csv"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(TargetImport.csv2targets)
        .compile
        .toList
        // .flatTap(x => IO(pprint.pprintln(x)))
        .map { l =>
          assertEquals(l.length, 20)
          assertEquals(l.count(_.isRight), 20)
        }
    }
  }

  test("parse stars with pm file") {
    val xmlFile = "/stars_pm.csv"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(TargetImport.csv2targets)
        .compile
        .toList
        // .flatTap(x => IO(pprint.pprintln(x)))
        .map { l =>
          assertEquals(l.length, 19)
          assertEquals(l.count(_.isRight), 19)
          assertEquals(l.count(_.exists(_.tracking.properMotion.isDefined)), 5)
        }
    }
  }

  test("parse stars with pm and epoch file") {
    val xmlFile = "/stars_pm_epoch.csv"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(TargetImport.csv2targets)
        .compile
        .toList
        // .flatTap(x => IO(pprint.pprintln(x)))
        .map { l =>
          assertEquals(l.length, 20)
          assertEquals(l.count(_.isRight), 19)
          assertEquals(l.count(_.exists(_.tracking.properMotion.isDefined)), 5)
          assertEquals(l.count(_.exists(_.tracking.epoch =!= Epoch.J2000)), 7)
          assertEquals(l.count(_.isLeft), 1)
          assertEquals(
            l.find(_.isLeft).map(_.leftMap(_.toList)),
            List(
              ImportProblem.CsvParsingError("Invalid epoch value 'J123' in line 18", 18L.some)
            ).asLeft.some
          )
        }
    }
  }

  test("parse stars sample file with errors") {
    val xmlFile = "/stars_with_errors.csv"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(TargetImport.csv2targets)
        .compile
        .toList
        .map { l =>
          assertEquals(l.length, 20)
          assertEquals(l.count(_.isRight), 19)
          assertEquals(l.count(_.isLeft), 1)
          assertEquals(
            l.find(_.isLeft).map(_.leftMap(_.toList)),
            List(
              ImportProblem.CsvParsingError("Invalid RA value 'a' in line 21", 21L.some)
            ).asLeft.some
          )
        }
    }
  }

  test("parse stars random file") {
    val xmlFile = "/random.csv"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(TargetImport.csv2targets)
        .compile
        .toList
        .map { l =>
          assertEquals(l.length, 1000)
          assertEquals(l.count(_.isRight), 1000)
        }
    }
  }

  test("parse names file with lookup") {
    val xmlFile = "/target_names.csv"
    val file    = getClass().getResource(xmlFile)
    JdkHttpClient
      .simple[IO]
      .flatMap { client =>
        Files[IO]
          .readAll(Path(file.getPath()))
          .through(text.utf8.decode)
          .through(TargetImport.csv2targetsAndLookup(client))
          .compile
          .toList
          // .flatTap(x => IO(pprint.pprintln(x)))
          .map { l =>
            assertEquals(l.length, 7)
            assertEquals(l.count(_.isRight), 4)
            assertEquals(l.count(_.isLeft), 3)
          }
      }
  }

  test("parse sample pit file with spaces") {
    val xmlFile = "/targets_test_case.csv"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(TargetImport.csv2targets)
        .compile
        .toList
        // .flatTap(x => IO(pprint.pprintln(x)))
        .map { l =>
          assertEquals(l.length, 1)
          assertEquals(
            l.head.toOption
              .flatMap(t => Target.integratedBrightnessIn(Band.V).headOption(t))
              .map(_.value),
            BrightnessValue.unsafeFrom(10).some
          )
        }
    }
  }

  test("test case with no pmRA") {
    val xmlFile = "/import_test_2.csv"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(TargetImport.csv2targets)
        .compile
        .toList
        // .flatTap(x => IO(pprint.pprintln(x)))
        .map { l =>
          assertEquals(l.length, 2)
          assertEquals(
            l.count {
              case Right(Target.Sidereal(_, s: SiderealTracking, _, _)) => s.properMotion.isEmpty
              case _                                                    => false
            },
            1
          )
        }
    }
  }
