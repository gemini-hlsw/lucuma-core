// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import fs2.*
import fs2.data.csv.*
import fs2.io.readClassLoaderResource
import lucuma.catalog.simbad.SEDDataConfig
import lucuma.catalog.simbad.SEDDataLoader
import lucuma.catalog.simbad.SEDMatcher
import lucuma.catalog.simbad.StellarLibraryParameters
import lucuma.catalog.simbad.StellarPhysics
import munit.CatsEffectSuite

case class SEDFixtureData(
  config:  SEDDataConfig,
  physics: StellarPhysics,
  library: StellarLibraryParameters,
  matcher: SEDMatcher
)

case class SimbadEntry(
  mainId:       String,
  otype:        String,
  morphType:    String,
  spectralType: String
)

case class ExpectedOutput(
  main_id:  String,
  otype:    String,
  filename: Option[String],
  t_eff:    Option[Double],
  log_g:    Option[Double]
)

trait SEDMatcherFixture:
  self: CatsEffectSuite =>

  given liftCellDecoder[A: CellDecoder]: CellDecoder[Option[A]] = s =>
    s.nonEmpty.guard[Option].traverse(_ => CellDecoder[A].apply(s))

  given CsvRowDecoder[SimbadEntry, String] = (row: CsvRow[String]) =>
    for
      mainId       <- row.as[String]("main_id")
      otype        <- row.as[String]("otype")
      morphType    <- row.as[String]("morph_type")
      spectralType <- row.as[String]("sp_type")
    yield SimbadEntry(mainId, otype, morphType, spectralType)

  given CsvRowDecoder[ExpectedOutput, String] = (row: CsvRow[String]) =>
    for
      mainId   <- row.as[String]("main_id")
      otype    <- row.as[String]("otype")
      filename <- row.as[Option[String]]("filename")
      tEff     <- row.as[Option[Double]]("t_eff")
      logG     <- row.as[Option[Double]]("log_g")
    yield ExpectedOutput(mainId, otype, filename, tEff, logG)

  val sedFixture = ResourceSuiteLocalFixture(
    "sedFixture",
    Resource.eval(SEDDataLoader.load.map { config =>
      val physics = new StellarPhysics(config.gravityTable)
      val library = new StellarLibraryParameters(config.stellarLibrary, physics)
      val matcher = new SEDMatcher(library, physics)
      SEDFixtureData(config, physics, library, matcher)
    })
  )

  def sedConfig: SEDDataConfig             = sedFixture().config
  def sedPhysics: StellarPhysics           = sedFixture().physics
  def sedLibrary: StellarLibraryParameters = sedFixture().library
  def sedMatcher: SEDMatcher               = sedFixture().matcher

  val testData: IO[List[SimbadEntry]] =
    readClassLoaderResource[IO]("simbad-sed-test-data-full.dat")
      .through(text.utf8.decode)
      .through(decodeUsingHeaders[SimbadEntry](' '))
      .compile
      .toList

  val expectedOutput: IO[List[ExpectedOutput]] =
    readClassLoaderResource[IO]("expected-output-full.csv")
      .through(text.utf8.decode)
      .through(decodeUsingHeaders[ExpectedOutput]())
      .compile
      .toList
