// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.data.EitherNec
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
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
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import munit.CatsEffectSuite

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneOffset

/**
 * Winners recorded with pyexplore test/blind-offset.py (version 2026-Sep-06) on the same fields.
 * The VOTables are the responses of the NOIRLab Gaia proxy to the query built by
 * `ADQLInterpreter.blindOffsetCandidates` for `Gaia3EsaProxy`, and to
 * `stellarParametersByIdQuery` for the four winners plus one star with ESP-HS values.
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
          observationTime
        )

  private def check(
    file:       String,
    instrument: Instrument,
    ra:         String,
    dec:        String,
    winner:     String
  ): IO[Unit] =
    candidates(file, instrument, coords(ra, dec)).map: sorted =>
      assert(sorted.nonEmpty)
      val best = sorted.head
      assertEquals(best.sourceId.value, s"Gaia DR3 $winner")
      assert(best.isUsable)
      // Usable candidates come first, in score order
      val usable = sorted.takeWhile(_.isUsable)
      assertEquals(usable.map(_.score), usable.map(_.score).sorted)
      assert(sorted.drop(usable.length).forall(!_.isUsable))

  test("GMOS 13:42:08.10 +09:28:38.4"):
    check("gaia-blind-offset-gmos1.xml", Instrument.GmosNorth, "13:42:08.10", "+09:28:38.4", "3725398080716580352")

  test("GNIRS 18:19:26.55 +38:45:01.8"):
    check("gaia-blind-offset-gnirs.xml", Instrument.Gnirs, "18:19:26.55", "+38:45:01.8", "2109057984352250368")

  test("GMOS 10:07:58.26 +21:15:29.2"):
    check("gaia-blind-offset-gmos2.xml", Instrument.GmosNorth, "10:07:58.26", "+21:15:29.2", "628705013665242240")

  test("Flamingos-2 03:03:31.41 -00:19:13.0"):
    check("gaia-blind-offset-f2.xml", Instrument.Flamingos2, "03:03:31.41", "-00:19:13.0", "3266527962404665088")

  test("stellar parameters are parsed from the proxy response"):
    candidates("gaia-blind-offset-gmos1.xml", Instrument.GmosNorth, coords("13:42:08.10", "+09:28:38.4"))
      .map: sorted =>
        assert(sorted.exists(_.stellarParameters.exists(_.source === StellarParametersSource.GspPhot)))

  private val HotStar: Long = 3726010543053126656L

  private def parsedStellarParameters: IO[Map[Long, StellarParameters]] =
    readClassLoaderResource[IO]("gaia-blind-offset-esphs.xml")
      .through(text.utf8.decode)
      .through(CatalogSearch.stellarParameters(CatalogAdapter.Gaia3EsaProxy))
      .compile
      .toList
      .map: results =>
        val (errors, params) = results.partitionEither(identity)
        assertEquals(errors, Nil)
        params.toMap

  test("ESP-HS id lookup parses only the stars that have values"):
    parsedStellarParameters.map: params =>
      assertEquals(
        params,
        Map(HotStar -> StellarParameters(8961, 3.227987, StellarParametersSource.EspHs))
      )

  // Serves the GMOS field from the fixture and reports ESP-HS values for the winner
  private def stubClient(
    requested: Ref[IO, List[Long]],
    winner:    Long
  ): GaiaClient[IO] =
    new GaiaClient[IO]:
      def query(adqlQuery: ADQLQuery)(using
        ADQLInterpreter
      ): IO[List[EitherNec[CatalogProblem, CatalogTargetResult]]] =
        readClassLoaderResource[IO]("gaia-blind-offset-gmos1.xml")
          .through(text.utf8.decode)
          .through(CatalogSearch.siderealTargets(CatalogAdapter.Gaia3EsaProxy))
          .compile
          .toList
      def queryById(sourceId: Long): IO[EitherNec[CatalogProblem, CatalogTargetResult]] =
        IO.raiseError(new NotImplementedError)
      def queryGuideStars(adqlQuery: ADQLQuery)(using
        ADQLInterpreter
      ): IO[List[EitherNec[CatalogProblem, Target.Sidereal]]] =
        IO.raiseError(new NotImplementedError)
      def queryByIdGuideStar(sourceId: Long): IO[EitherNec[CatalogProblem, Target.Sidereal]] =
        IO.raiseError(new NotImplementedError)
      def queryStellarParameters(sourceIds: List[Long]): IO[Map[Long, StellarParameters]] =
        requested.set(sourceIds).as(
          Map(winner -> StellarParameters(11531, 4.07, StellarParametersSource.EspHs))
        )

  test("ESP-HS values are fetched for the best candidates and replace GSP-Phot"):
    val winner = 3725398080716580352L
    for
      requested <- Ref.of[IO, List[Long]](Nil)
      sorted    <- BlindOffsets.runBlindOffsetAnalysis(
                     stubClient(requested, winner),
                     sedMatcher,
                     Instrument.GmosNorth,
                     coords("13:42:08.10", "+09:28:38.4"),
                     observationTime
                   )
      ids       <- requested.get
    yield
      assertEquals(ids.length, BlindOffsets.HotStarLookups)
      assertEquals(ids.headOption, Some(winner))
      assert(sorted.take(ids.length).forall(_.isUsable))
      val best = sorted.head
      assertEquals(best.sourceId.value, s"Gaia DR3 $winner")
      assertEquals(best.stellarParameters.map(_.source), Some(StellarParametersSource.EspHs))
      val sed  = SourceProfile.unnormalizedSED.getOption(best.catalogResult.target.sourceProfile).flatten
      assertEquals(sed, Some(UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V_new)))
      // Everyone else is untouched
      assert(sorted.tail.forall(_.stellarParameters.forall(_.source == StellarParametersSource.GspPhot)))

  test("instruments without limits return no candidates and query nothing"):
    for
      requested <- Ref.of[IO, List[Long]](List(1L))
      sorted    <- BlindOffsets.runBlindOffsetAnalysis(
                     stubClient(requested, 0L),
                     sedMatcher,
                     Instrument.MaroonX,
                     coords("13:42:08.10", "+09:28:38.4"),
                     observationTime
                   )
      ids       <- requested.get
    yield
      assertEquals(sorted, Nil)
      assertEquals(ids, List(1L))
