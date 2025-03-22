// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.MonadThrow
import cats.effect.*
import cats.syntax.all.*
import fs2.*
import fs2.data.xml.*
import lucuma.catalog.*
import munit.CatsEffectSuite

import scala.xml.Node
import scala.xml.Utility

class VoTableParserSuite extends CatsEffectSuite with VoTableParser with VoTableSamples {
  def toStream[F[_]: MonadThrow](xml: Node): Stream[F, XmlEvent] =
    Stream
      .emits(Utility.trim(xml).toString)
      .through(events[F, Char]())
      .through(referenceResolver[F]())
      .through(normalize[F])

  test("parse a list of rows") {
    val result = List(
      TableRow(
        List(
          TableRowItem(FieldId.unsafeFrom("flags1", "meta.code"), "268435728"),
          TableRowItem(FieldId.unsafeFrom("umag", "phot.mag;em.opt.u"), "23.0888"),
          TableRowItem(FieldId.unsafeFrom("flags2", "meta.code"), "8208"),
          TableRowItem(FieldId.unsafeFrom("imag", "phot.mag;em.opt.i"), "20.3051"),
          TableRowItem(FieldId.unsafeFrom("decj2000", "pos.eq.dec;meta.main"), "0.209323681906"),
          TableRowItem(FieldId.unsafeFrom("raj2000", "pos.eq.ra;meta.main"), "359.745951955"),
          TableRowItem(FieldId.unsafeFrom("rmag", "phot.mag;em.opt.r"), "20.88"),
          TableRowItem(FieldId.unsafeFrom("objid", "meta.id;meta.main"), "-2140405448"),
          TableRowItem(FieldId.unsafeFrom("gmag", "phot.mag;em.opt.g"), "22.082"),
          TableRowItem(FieldId.unsafeFrom("zmag", "phot.mag;em.opt.z"), "19.8812"),
          TableRowItem(FieldId.unsafeFrom("type", "meta.code"), "3"),
          TableRowItem(FieldId.unsafeFrom("ppmxl", "meta.id;meta.main"), "-2140405448")
        )
      ).rightNec
    )

    toStream[IO](targets)
      .through(trsf)
      .compile
      .toList
      .map(v => assertEquals(v.headOption, result.headOption))
  }
}
