// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import cats.syntax.all.*
import fs2.*
import fs2.data.csv.*
import fs2.io.readClassLoaderResource
import lucuma.catalog.simbad.SpectralTypeParsers
import lucuma.catalog.simbad.StellarLibraryParameters
import lucuma.catalog.simbad.StellarPhysics
import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED

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

object SEDValidationData:

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

  def filenameToSED(filename: String): Option[UnnormalizedSED] =
    filename match
      case "QSO.sed"        => Some(UnnormalizedSED.Quasar(QuasarSpectrum.QS0))
      case "HII.sed"        => Some(UnnormalizedSED.HIIRegion(HIIRegionSpectrum.OrionNebula))
      case "PN.sed"         => Some(UnnormalizedSED.PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009))
      case "Elliptical.sed" => Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
      case "Spiral.sed"     => Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral))
      case stellar          =>
        val spectrumTag = stellar.stripSuffix(".nm")
        StellarLibrarySpectrum.values
          .find(_.tag === spectrumTag)
          .map(UnnormalizedSED.StellarLibrary(_))

  def stellarBaseType(sed: UnnormalizedSED): Option[String] =
    sed match
      case UnnormalizedSED.StellarLibrary(spectrum) =>
        spectrum.toString.stripSuffix("_new").some
      case _                                        =>
        none

  def sedEquivalent(p: UnnormalizedSED, s: UnnormalizedSED): Boolean =
    (stellarBaseType(p), stellarBaseType(s)) match
      case (Some(pBase), Some(sBase)) => pBase === sBase
      case _                          => p === s

  def isScoreTie(
    spectralType: String,
    expectedSED:  UnnormalizedSED,
    liveSED:      UnnormalizedSED,
    physics:      StellarPhysics,
    library:      StellarLibraryParameters
  ): Boolean =
    (expectedSED, liveSED) match
      case (UnnormalizedSED.StellarLibrary(pSpectrum), UnnormalizedSED.StellarLibrary(sSpectrum)) =>
        val cleaned = spectralType.replaceAll("[():]", "")
        SpectralTypeParsers.spectralType
          .parse(cleaned)
          .toOption
          .flatMap { case (_, (lum, temp)) =>
            physics
              .calculateParameters(lum, temp)
              .flatMap { targetParams =>
                val pParams = library.params.get(pSpectrum)
                val sParams = library.params.get(sSpectrum)
                (pParams, sParams).mapN { (pp, sp) =>
                  val dtMax = 0.1 * targetParams.temp.value
                  val dgMax = 0.5
                  def score(libParams: StellarPhysics.StellarParameters): Double =
                    val dt = (libParams.temp.value - targetParams.temp.value).toDouble / dtMax
                    val dg = (libParams.logG - targetParams.logG) / dgMax
                    math.sqrt(dt * dt + dg * dg)
                  val pScore = score(pp)
                  val sScore = score(sp)
                  math.abs(pScore - sScore) / math.max(pScore, sScore) < 0.01
                }
              }
          }
          .getOrElse(false)
      case _ => false
