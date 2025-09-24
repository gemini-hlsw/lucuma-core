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
