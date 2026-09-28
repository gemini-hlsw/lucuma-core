// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import cats.Eq
import lucuma.catalog.goa.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceMode
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbAngle.given
import lucuma.core.math.arb.ArbCoordinates.given
import lucuma.core.util.Enumerated
import lucuma.core.util.arb.ArbEnumerated.given
import monocle.law.discipline.OptionalTests
import munit.DisciplineSuite
import org.http4s.Uri
import org.http4s.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class GoaEndpointSuite extends DisciplineSuite:

  private val goaInstruments: List[Instrument] =
    Enumerated[Instrument].all.filter(_.goaName.isDefined)

  private val genParams: Gen[GoaParams] =
    for
      instrument <- Gen.oneOf(goaInstruments)
      radius     <- Arbitrary.arbitrary[Angle]
      mode       <- Gen.option(Gen.oneOf(ScienceMode.Imaging, ScienceMode.Spectroscopy))
      params     <- Gen.oneOf(
                      Arbitrary
                        .arbitrary[Coordinates]
                        .map(GoaParams.Sidereal(_, instrument, radius, mode)),
                      Gen.alphaNumStr.map(GoaParams.NonSidereal(_, instrument, radius, mode))
                    )
    yield params

  private val bases: List[Uri] =
    List(GoaClient.DefaultBaseUri, uri"http://localhost:8080/goa")

  private val genQueryUri: Gen[(GoaParams, GoaEndpoint, Uri)] =
    for
      params   <- genParams
      endpoint <- Arbitrary.arbitrary[GoaEndpoint]
      base     <- Gen.oneOf(bases)
    yield (params, endpoint, GoaParams.toUri(params, base, endpoint).get)

  private val genOtherUri: Gen[Uri] =
    Gen.oneOf(
      uri"https://archive.gemini.edu/",
      uri"https://archive.gemini.edu/jsonsummary/",
      uri"https://archive.gemini.edu/searchform/GMOS-N",
      uri"https://example.com/jsonsummary/engineering/NotFail/GMOS-N"
    )

  given Arbitrary[Uri] = Arbitrary(Gen.oneOf(genQueryUri.map(_._3), genOtherUri))
  given Eq[Uri]        = Eq.by(_.renderString)

  checkAll("GoaEndpoint.fromUri", OptionalTests(GoaEndpoint.fromUri))

  property("fromUri reads the endpoint toUri was given"):
    forAll(genQueryUri): (_, endpoint, uri) =>
      assertEquals(GoaEndpoint.fromUri.getOption(uri), Some(endpoint))

  property("retargeting a query is building it on the other endpoint"):
    forAll(genQueryUri): (params, _, uri) =>
      val base = bases.find(b => uri.renderString.startsWith(b.renderString)).get
      assertEquals(
        GoaEndpoint.fromUri.replace(GoaEndpoint.SearchForm)(uri),
        GoaParams.toUri(params, base, GoaEndpoint.SearchForm).get
      )

  property("instrumentOf reads the instrument toUri was given"):
    forAll(genQueryUri): (params, _, uri) =>
      assertEquals(GoaParams.instrumentOf(uri), Some(params.instrument))

  test("a URL that is not a GOA query has no endpoint or instrument"):
    val other = uri"https://archive.gemini.edu/searchform/GMOS-N"
    assertEquals(GoaEndpoint.fromUri.getOption(other), None)
    assertEquals(GoaParams.instrumentOf(other), None)
