// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import cats.Show
import lucuma.core.parser.MiscParsers
import lucuma.core.syntax.all.*
import monocle.Prism

/** A positive, non-zero value for numbered identifiers. */
sealed abstract case class Index(toShort: Short)
object Index extends IndexOptics {

  val One: Index =
    fromShort.unsafeGet(1)

  given Order[Index] =
    Order.by(_.toShort)

  given scala.math.Ordering[Index] =
    summon[Order[Index]].toOrdering

  given Show[Index] =
    Show.fromToString

}

trait IndexOptics {

  /** @group Optics */
  val fromShort: Prism[Short, Index]   =
    Prism((i: Short) => Option(i).filter(_ > 0).map(new Index(_) {}))(_.toShort)

  /** @group Optics */
  val fromString: Prism[String, Index] =
    Prism[String, Index](MiscParsers.index.parseAll(_).toOption)(_.toShort.toString)

}
