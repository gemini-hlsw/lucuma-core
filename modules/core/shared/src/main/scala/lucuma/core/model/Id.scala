// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.util.Gid

/**
 * Top-level shareable object ids.
 */
object Id {

  final case class Asterism(value: PosLong) {
    override def toString: String =
      Gid[Asterism].show(this)
  }

  object Asterism {
    implicit val GidAsterism: Gid[Asterism] =
      Gid.instance('a', _.value, apply)
  }

  final case class Observation(value: PosLong) {
    override def toString: String =
      Gid[Observation].show(this)
  }

  object Observation {
    implicit val GidObservation: Gid[Observation] =
      Gid.instance('o', _.value, apply)
  }

  final case class Program(value: PosLong) {
    override def toString: String =
      Gid[Program].show(this)
  }

  object Program {
    implicit val GidProgram: Gid[Program] =
      Gid.instance('p', _.value, apply)
  }

  final case class Target(value: PosLong) {
    override def toString: String =
      Gid[Target].show(this)
  }

  object Target {
    implicit val GidTarget: Gid[Target] =
      Gid.instance('t', _.value, apply)
  }

}
