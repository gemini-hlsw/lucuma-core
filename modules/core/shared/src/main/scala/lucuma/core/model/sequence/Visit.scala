// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.auto._
import lucuma.core.util.WithUid
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism
import org.typelevel.cats.time._

import java.time.Duration
import java.time.Instant

sealed trait Visit {
  def id: Visit.Id
  def created: Instant
  def staticConfig: StaticConfig
  def steps: List[StepRecord]

  def startTime: Option[Instant] = steps.headOption.flatMap(_.startTime)
  def endTime: Option[Instant]   = steps.lastOption.flatMap(_.endTime)

  def duration: Option[Duration] =
    (startTime, endTime).mapN(Duration.between)
}

object Visit extends WithUid('v') {
  final case class GmosNorth(
    val id:           Visit.Id,
    val created:      Instant,
    val staticConfig: StaticConfig.GmosNorth,
    val steps:        List[StepRecord.GmosNorth]
  ) extends Visit
  object GmosNorth {
    implicit val eqVisitGmosNorth: Eq[GmosNorth] =
      Eq.by(x => (x.id, x.created, x.staticConfig, x.steps))

    val id: Lens[GmosNorth, Visit.Id] = Focus[GmosNorth](_.id)

    val created: Lens[GmosNorth, Instant] = Focus[GmosNorth](_.created)

    val staticConfig: Lens[GmosNorth, StaticConfig.GmosNorth] = Focus[GmosNorth](_.staticConfig)

    val steps: Lens[GmosNorth, List[StepRecord.GmosNorth]] = Focus[GmosNorth](_.steps)
  }

  final case class GmosSouth(
    val id:           Visit.Id,
    val created:      Instant,
    val staticConfig: StaticConfig.GmosSouth,
    val steps:        List[StepRecord.GmosSouth]
  ) extends Visit
  object GmosSouth {
    implicit val eqVisitGmosSouth: Eq[GmosSouth] =
      Eq.by(x => (x.id, x.created, x.staticConfig, x.steps))

    val id: Lens[GmosSouth, Visit.Id] = Focus[GmosSouth](_.id)

    val created: Lens[GmosSouth, Instant] = Focus[GmosSouth](_.created)

    val staticConfig: Lens[GmosSouth, StaticConfig.GmosSouth] = Focus[GmosSouth](_.staticConfig)

    val steps: Lens[GmosSouth, List[StepRecord.GmosSouth]] = Focus[GmosSouth](_.steps)
  }

  implicit val eqVisit: Eq[Visit] = Eq.instance {
    case (a @ GmosNorth(_, _, _, _), b @ GmosNorth(_, _, _, _)) => a === b
    case (a @ GmosSouth(_, _, _, _), b @ GmosSouth(_, _, _, _)) => a === b
    case _                                                      => false
  }

  val gmosNorth: Prism[Visit, GmosNorth] = GenPrism[Visit, GmosNorth]

  val gmosSouth: Prism[Visit, GmosSouth] = GenPrism[Visit, GmosSouth]
}
