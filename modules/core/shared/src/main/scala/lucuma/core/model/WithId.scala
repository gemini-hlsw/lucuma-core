// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.util.Gid
import eu.timepit.refined.api.Refined
import eu.timepit.refined.char.Letter
import eu.timepit.refined.types.numeric.PosLong

/**
 * Defines `<Entity>.Id` class, its `Gid` instance, and convenience methods.
 */
trait WithId {

  /** Tag for Gid instance */
  protected val idTag: Char Refined Letter

  /** Id class for `<Entity>` */
  case class Id(value: PosLong) {
    override def toString: String =
      Gid[Id].show(this)
  }

  object Id {

    /** @group Typeclass Instances */
    implicit val GidId: Gid[Id] = Gid.instance(idTag, _.value, apply)

    /** Convenience method to construct from a Long */
    def fromLong(l: Long): Option[Id] = GidId.fromLong.getOption(l)

    /** Convenience method to construct from a String */
    def parse(s: String): Option[Id] = GidId.fromString.getOption(s)

    /** Allow pattern match style parsing */
    def unapply[T](s: String): Option[Id] = parse(s)
  }
}
