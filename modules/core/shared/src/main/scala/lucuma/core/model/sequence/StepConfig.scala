// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.all._
import lucuma.core.enums._
import lucuma.core.math.Offset
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed abstract class StepConfig(val stepType: StepType)
object StepConfig {
  case object Bias extends StepConfig(StepType.Bias)
  case object Dark extends StepConfig(StepType.Dark)
  final case class Gcal(
    continuum: Option[GcalContinuum],
    arcs:      List[GcalArc],
    filter:    GcalFilter,
    diffuser:  GcalDiffuser,
    shutter:   GcalShutter
  ) extends StepConfig(StepType.Gcal)
  object Gcal {
    implicit val eqStepConfigGcal: Eq[Gcal] =
      Eq.by(x => (x.continuum, x.arcs, x.filter, x.diffuser, x.shutter))

    /** @group Optics */
    val continuum: Lens[Gcal, Option[GcalContinuum]] =
      Focus[Gcal](_.continuum)

    /** @group Optics */
    val arcs: Lens[Gcal, List[GcalArc]] =
      Focus[Gcal](_.arcs)

    /** @group Optics */
    val filter: Lens[Gcal, GcalFilter] =
      Focus[Gcal](_.filter)

    /** @group Optics */
    val diffuser: Lens[Gcal, GcalDiffuser] =
      Focus[Gcal](_.diffuser)

    /** @group Optics */
    val shutter: Lens[Gcal, GcalShutter] =
      Focus[Gcal](_.shutter)
  }

  final case class Science(offset: Offset) extends StepConfig(StepType.Science)
  object Science {
    implicit val eqStepConfigScience: Eq[Science] = Eq.by(_.offset)

    /** @group Optics */
    val offset: Lens[Science, Offset] =
      Focus[Science](_.offset)
  }

  implicit val eqStepConfig: Eq[StepConfig] = Eq.instance {
    case (Bias, Bias)                                       => true
    case (Dark, Dark)                                       => true
    case (a @ Gcal(_, _, _, _, _), b @ Gcal(_, _, _, _, _)) => a === b
    case (a @ Science(_), b @ Science(_))                   => a === b
    case _                                                  => false
  }

  /** @group Optics */
  val gcal: Prism[StepConfig, Gcal] =
    GenPrism[StepConfig, Gcal]

  /** @group Optics */
  val science: Prism[StepConfig, Science] =
    GenPrism[StepConfig, Science]
}
