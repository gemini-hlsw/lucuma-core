// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.data.EitherNec
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import coulomb.syntax.*
import coulomb.units.si.Kelvin
import fs2.io.readClassLoaderResource
import fs2.text
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.votable.ADQLInterpreter
import lucuma.catalog.votable.ADQLQuery
import lucuma.catalog.votable.CatalogAdapter
import lucuma.catalog.votable.CatalogProblem
import lucuma.catalog.votable.CatalogSearch
import lucuma.core.enums.Instrument
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED
import munit.CatsEffectSuite

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneOffset

/**
 * Rankings and SEDs recorded with pyexplore test/blind-offset.py (version 2026-Sep-06) on the same
 * fields, on 2026-10-09. The VOTables are the responses of the NOIRLab Gaia proxy to the query
 * built by `ADQLInterpreter.blindOffsetCandidates` for `Gaia3EsaProxy`. They also carry
 * teff_gspphot and logg_gspphot columns, which the parser ignores.
 */
class BlindOffsetRegressionSuite extends CatsEffectSuite with SEDMatcherFixture:

  override def munitFixtures = List(sedFixture)

  private val observationTime: Instant =
    LocalDate.of(2026, 9, 6).atStartOfDay(ZoneOffset.UTC).toInstant()

  private def coords(ra: String, dec: String): Coordinates =
    (RightAscension.fromStringHMS.getOption(ra), Declination.fromStringSignedDMS.getOption(dec))
      .mapN(Coordinates.apply)
      .get

  private def candidates(
    file:       String,
    instrument: Instrument,
    base:       Coordinates
  ): IO[List[BlindOffsetCandidate]] =
    readClassLoaderResource[IO](file)
      .through(text.utf8.decode)
      .through(CatalogSearch.siderealTargets(CatalogAdapter.Gaia3EsaProxy))
      .compile
      .toList
      .map: results =>
        val (errors, targets) = results.partitionEither(identity)
        assertEquals(errors, Nil)
        BlindOffsets.analysis(
          targets,
          sedMatcher,
          BlindOffsetLimits.forInstrument(instrument).get,
          base,
          observationTime,
          Map.empty
        )

  // Serves a field from its fixture, and answers lookups with `lookup`
  private def stubClient(
    field:  String,
    lookup: List[Long] => IO[Map[Long, GaiaStellarParameters]]
  ): GaiaClient[IO] =
    new GaiaClient[IO]:
      def query(adqlQuery: ADQLQuery)(using
        ADQLInterpreter
      ): IO[List[EitherNec[CatalogProblem, CatalogTargetResult]]]                             =
        readClassLoaderResource[IO](field)
          .through(text.utf8.decode)
          .through(CatalogSearch.siderealTargets(CatalogAdapter.Gaia3EsaProxy))
          .compile
          .toList
      def queryById(sourceId: Long): IO[EitherNec[CatalogProblem, CatalogTargetResult]]       =
        IO.raiseError(new NotImplementedError)
      def queryGuideStars(adqlQuery: ADQLQuery)(using
        ADQLInterpreter
      ): IO[List[EitherNec[CatalogProblem, Target.Sidereal]]]                                 =
        IO.raiseError(new NotImplementedError)
      def queryByIdGuideStar(sourceId: Long): IO[EitherNec[CatalogProblem, Target.Sidereal]]  =
        IO.raiseError(new NotImplementedError)
      def queryStellarParameters(sourceIds: List[Long]): IO[Map[Long, GaiaStellarParameters]] =
        lookup(sourceIds)

  // The NOIRLab proxy's answer to the lookup for the four winners
  private def recordedLookup(sourceIds: List[Long]): IO[Map[Long, GaiaStellarParameters]] =
    readClassLoaderResource[IO]("gaia-stellar-parameters-esa.xml")
      .through(text.utf8.decode)
      .through(CatalogSearch.stellarParameters(CatalogAdapter.Gaia3EsaProxy))
      .compile
      .toList
      .map(_.collect { case Right(p) => p }.toMap.view.filterKeys(sourceIds.contains).toMap)

  private def sedOf(c: BlindOffsetCandidate): Option[UnnormalizedSED] =
    SourceProfile.unnormalizedSED.getOption(c.catalogResult.target.sourceProfile).flatten

  // Top five usable candidates and the winner's SED as chosen by pyexplore, except where noted
  private def check(
    file:       String,
    instrument: Instrument,
    ra:         String,
    dec:        String,
    topFive:    List[String],
    winnerSed:  UnnormalizedSED
  ): IO[Unit] =
    BlindOffsets
      .runBlindOffsetAnalysis(stubClient(file, recordedLookup),
                              sedMatcher,
                              instrument,
                              coords(ra, dec),
                              observationTime
      )
      .map: sorted =>
        val usable = sorted.takeWhile(_.isUsable)
        assertEquals(usable.take(5).map(_.sourceId.value), topFive.map(id => s"Gaia DR3 $id"))
        assertEquals(sedOf(sorted.head), Some(winnerSed))
        // Usable candidates come first, in score order
        assertEquals(usable.map(_.score), usable.map(_.score).sorted)
        assert(sorted.drop(usable.length).forall(!_.isUsable))

  private def library(s: StellarLibrarySpectrum) = UnnormalizedSED.StellarLibrary(s)

  test("GMOS 13:42:08.10 +09:28:38.4"):
    check(
      "gaia-blind-offset-gmos1.xml",
      Instrument.GmosNorth,
      "13:42:08.10",
      "+09:28:38.4",
      List("3725398080716580352",
           "3725409449494652672",
           "3725409861811527680",
           "3725409866106841344",
           "3725397389226454400"
      ),
      library(StellarLibrarySpectrum.F8V)
    )

  test("GNIRS 18:19:26.55 +38:45:01.8"):
    check(
      "gaia-blind-offset-gnirs.xml",
      Instrument.Gnirs,
      "18:19:26.55",
      "+38:45:01.8",
      List("2109057984352250368",
           "2109034520949264512",
           "2109034722809354368",
           "2109034516650915840",
           "2109057988650576512"
      ),
      library(StellarLibrarySpectrum.M0V_new)
    )

  test("GMOS 10:07:58.26 +21:15:29.2"):
    check(
      "gaia-blind-offset-gmos2.xml",
      Instrument.GmosNorth,
      "10:07:58.26",
      "+21:15:29.2",
      List("628705013665242240",
           "628703703699947904",
           "628703772419421696",
           "628703467477016576",
           "628728378287329664"
      ),
      library(StellarLibrarySpectrum.F8V)
    )

  test("Flamingos-2 03:03:31.41 -00:19:13.0"):
    // pyexplore ranks 3266527962404526720 4th and 3266531020421383424 6th; both fail the
    // astrometric quality cuts here. The winner has no Teff or log g.
    check(
      "gaia-blind-offset-f2.xml",
      Instrument.Flamingos2,
      "03:03:31.41",
      "-00:19:13.0",
      List("3266527962404665088",
           "3266528035419596160",
           "3266528065483747328",
           "3266528065483747712",
           "3266527653166877056"
      ),
      UnnormalizedSED.PowerLaw(BigDecimal(0))
    )

  // Reports stellar parameters for the GMOS winner only, or fails
  private def winnerOnly(
    requested:  Ref[IO, List[Long]],
    winner:     Long,
    failLookup: Boolean = false
  ): GaiaClient[IO] =
    stubClient(
      "gaia-blind-offset-gmos1.xml",
      ids =>
        requested.set(ids) *> IO.raiseWhen(failLookup)(new RuntimeException("lookup")) *>
          IO.pure(
            Map(
              winner -> GaiaStellarParameters(11531.withUnit[Kelvin],
                                              4.07,
                                              GaiaStellarParametersSource.EspHs
              )
            )
          )
    )

  test("stellar parameters are looked up for the best candidates only"):
    val winner = 3725398080716580352L
    for
      requested <- Ref.of[IO, List[Long]](Nil)
      plain     <- candidates(
                     "gaia-blind-offset-gmos1.xml",
                     Instrument.GmosNorth,
                     coords("13:42:08.10", "+09:28:38.4")
                   )
      sorted    <- BlindOffsets.runBlindOffsetAnalysis(
                     winnerOnly(requested, winner),
                     sedMatcher,
                     Instrument.GmosNorth,
                     coords("13:42:08.10", "+09:28:38.4"),
                     observationTime
                   )
      ids       <- requested.get
    yield
      assertEquals(ids.length, BlindOffsets.StellarParameterLookups)
      assertEquals(ids.headOption, Some(winner))
      assert(sorted.take(ids.length).forall(_.isUsable))
      // Teff does not enter the score
      assertEquals(sorted.map(_.sourceId), plain.map(_.sourceId))
      val best = sorted.head
      assertEquals(best.sourceId.value, s"Gaia DR3 $winner")
      assertEquals(best.stellarParameters.map(_.source), Some(GaiaStellarParametersSource.EspHs))
      assertEquals(sedOf(best), Some(library(StellarLibrarySpectrum.A0V_new)))
      // Everyone else keeps the power law
      assert(sorted.tail.forall(_.stellarParameters.isEmpty))
      assert(
        sorted.tail.forall(sedOf(_) === Some(UnnormalizedSED.PowerLaw(BigDecimal(0))))
      )

  test("a failed lookup leaves every candidate with the power law"):
    for
      requested <- Ref.of[IO, List[Long]](Nil)
      expected  <- candidates(
                     "gaia-blind-offset-gmos1.xml",
                     Instrument.GmosNorth,
                     coords("13:42:08.10", "+09:28:38.4")
                   )
      sorted    <- BlindOffsets.runBlindOffsetAnalysis(
                     winnerOnly(requested, 3725398080716580352L, failLookup = true),
                     sedMatcher,
                     Instrument.GmosNorth,
                     coords("13:42:08.10", "+09:28:38.4"),
                     observationTime
                   )
      ids       <- requested.get
    yield
      assertEquals(ids.length, BlindOffsets.StellarParameterLookups)
      assertEquals(sorted, expected)
      assert(sorted.forall(_.stellarParameters.isEmpty))

  test("instruments without limits return no candidates and query nothing"):
    for
      requested <- Ref.of[IO, List[Long]](List(1L))
      sorted    <- BlindOffsets.runBlindOffsetAnalysis(
                     winnerOnly(requested, 0L),
                     sedMatcher,
                     Instrument.MaroonX,
                     coords("13:42:08.10", "+09:28:38.4"),
                     observationTime
                   )
      ids       <- requested.get
    yield
      assertEquals(sorted, Nil)
      assertEquals(ids, List(1L))
