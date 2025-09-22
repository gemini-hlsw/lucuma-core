// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import cats.effect.IO
import cats.syntax.all.*

trait HorizonsClientSearchSuite[A](ctor: String => HorizonsClient.Search[A]) extends HorizonsClientSuite:

  def testEmptyResults(partial: String, detail: Option[String] = None) =
    test(s"empty results${detail.foldMap(" - " + _)}"):
      client.use: c =>
        assertIO(
          c.resolve(ctor(partial)),
          Right(Nil)
        )

  def testMultipleResults(partial: String, expected: (A, String)*) =
    test("multiple results"):
      client.use: c =>
        assertIO(
          c.resolve(ctor(partial)).map(_.map(_.take(expected.length))),
          Right(expected.toList)
        )

  def testSingleResult(style: String)(partial: String, expected: (A, String)) =
    test(s"single result - $style"):
      client.use: c =>
        assertIO(
          c.resolve(ctor(partial)),
          Right(List(expected))
        )


//   "ephemeris lookup" should {

//     "return a populated ephemeris for Halley (comet)" in {
//       runLookup(HD.Comet("1P"), 100).map(_.size).toOption.exists { s =>
//         95 <= s && s <= 105
//       }
//     }

//     "return a populated ephemeris for Sedna (asteroid, new style)" in {
//       runLookup(HD.AsteroidNewStyle("2003 VB12"), 100).map(_.size).toOption.exists { s =>
//         95 <= s && s <= 105
//       }
//     }

//     "return a populated ephemeris for 'Oumuamua (asteroid, new style)" in {
//       runLookup(HD.AsteroidNewStyle("A/2017 U1"), 100).map(_.size).toOption.exists { s =>
//         95 <= s && s <= 105
//       }
//     }

//     "return a populated ephemeris for Amphitrite (asteroid, old style)" in {
//       runLookup(HD.AsteroidOldStyle(29), 100).map(_.size).toOption.exists { s =>
//         95 <= s && s <= 105
//       }
//     }

//     "return a populated ephemeris for Charon (major body)" in {
//       runLookup(HD.MajorBody(901), 100).map(_.size).toOption.exists { s =>
//         95 <= s && s <= 105
//       }
//     }

//     "return an empty ephemeris on bogus lookup" in {
//       runLookup(HD.Comet("29134698698376"), 100).map(_.size) must_== -\/(EphemerisEmpty)
//     }

//   }

//   "ephemeris element lookup" should {

//     // sanity check with a known ephemeris, to catch changes in output format
//     "return a correct ephemeris for Halley (comet)" in {
//       val result =
//         runLookupE(HD.Comet("1P"), 10).toOption.map(_.toList.map(_.toString.replace('\t', ' ')))

//       // result.foreach(_.foreach(println))
//       result must_== Some(
//         List(
//           "(1675184400000,2023-Jan-31 17:00:00 UTC 08:17:44.423 +02:15:52.57 -4.29131 1.183281 -1.0 25.535)",
//           "(1676748600000,2023-Feb-18 19:30:00 UTC 08:15:43.624 +02:25:30.99 -3.88567 1.474407 -1.0 25.54)",
//           "(1678312800000,2023-Mar-08 22:00:00 UTC 08:14:01.535 +02:36:42.31 -3.10269 1.605413 2.3 25.55)",
//           "(1679877000000,2023-Mar-27 00:30:00 UTC 08:12:47.94 +02:48:16.16 -2.01608 1.573011 1.192 25.565)",
//           "(1681441200000,2023-Apr-14 03:00:00 UTC 08:12:09.054 +02:59:03.78 -0.74028 1.38962 2.016 25.583)",
//           "(1683005400000,2023-May-02 05:30:00 UTC 08:12:07.023 +03:08:06.46 0.577456 1.085775 -1.0 25.601)",
//           "(1684569600000,2023-May-20 08:00:00 UTC 08:12:40.294 +03:14:39.09 1.767352 0.701981 -1.0 25.619)",
//           "(1686133800000,2023-Jun-07 10:30:00 UTC 08:13:44.373 +03:18:12.91 2.707549 0.271758 -1.0 25.634)",
//           "(1687698000000,2023-Jun-25 13:00:00 UTC 08:15:12.407 +03:18:36.17 3.351082 -0.17043 20.718 25.646)",
//           "(1689262200000,2023-Jul-13 15:30:00 UTC 08:16:56.008 +03:15:51.9 3.688434 -0.58833 1.412 25.653)",
//           "(1690826400000,2023-Jul-31 18:00:00 UTC 08:18:45.969 +03:10:17.27 3.734413 -0.95298 1.306 25.655)"
//         )
//       )
//     }

//   }

// }
