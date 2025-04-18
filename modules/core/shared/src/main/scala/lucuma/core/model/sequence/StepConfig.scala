// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptySet
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.*
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism
import monocle.std.either.*

import scala.collection.immutable.SortedSet

sealed abstract class StepConfig(val stepType: StepType):
  def usesGcalUnit: Boolean =
    stepType === StepType.Gcal || stepType === StepType.SmartGcal

object StepConfig:

  case object Bias extends StepConfig(StepType.Bias)

  case object Dark extends StepConfig(StepType.Dark)

  final case class Gcal(
    lamp:      Gcal.Lamp,
    filter:    GcalFilter,
    diffuser:  GcalDiffuser,
    shutter:   GcalShutter
  ) extends StepConfig(StepType.Gcal) derives Eq:

    def lampType: GcalLampType =
      lamp.lampType

  object Gcal:
    opaque type Lamp = Either[GcalContinuum, NonEmptySet[GcalArc]]

    object Lamp:
      extension (lamp: Lamp)
        def toEither: Either[GcalContinuum, NonEmptySet[GcalArc]] =
          lamp

        def fold[A](fc: GcalContinuum => A, fas: NonEmptySet[GcalArc] => A): A =
          lamp.fold(fc, fas)

        def lampType: GcalLampType =
          lamp.fold(_ => GcalLampType.Flat, _ => GcalLampType.Arc)

        def continuum: Option[GcalContinuum] =
          lamp.swap.toOption

        def arcs: Option[NonEmptySet[GcalArc]] =
          lamp.toOption

        def toArcsSortedSet: SortedSet[GcalArc] =
          arcs.fold(SortedSet.empty[GcalArc])(_.toSortedSet)

      def fromEither(e: Either[GcalContinuum, NonEmptySet[GcalArc]]): Lamp =
        e

      def fromContinuum(c: GcalContinuum): Lamp =
        c.asLeft[NonEmptySet[GcalArc]]

      def fromArcs(as: NonEmptySet[GcalArc]): Lamp =
        as.asRight[GcalContinuum]

      def fromContinuumOrArcs(
        continuum: Option[GcalContinuum],
        arcs:      Iterable[GcalArc]
      ): Either[String, Lamp] =
        (continuum, arcs.toList) match {
          case (None, Nil)        => "Exactly one of continuum or arcs must be provided, received neither".asLeft
          case (Some(_), a :: as) => "Exactly one of continuum or arcs must be provided, received both".asLeft
          case (Some(u), _)       => u.asLeft[NonEmptySet[GcalArc]].asRight
          case (_, a :: as)       => NonEmptySet.of(a, as*).asRight[GcalContinuum].asRight
        }

      given Eq[Lamp] =
        Eq.by { lamp => (lamp.continuum, lamp.arcs) }

    end Lamp

    /** @group Optics */
    val lamp: Lens[Gcal, Either[GcalContinuum, NonEmptySet[GcalArc]]] =
      Focus[Gcal](_.lamp)

    /** @group Optics */
    val continuum: Optional[Gcal, GcalContinuum] =
      lamp.andThen(stdLeft)

    /** @group Optics */
    val arcs: Optional[Gcal, NonEmptySet[GcalArc]] =
      lamp.andThen(stdRight)

    /** @group Optics */
    val filter: Lens[Gcal, GcalFilter] =
      Focus[Gcal](_.filter)

    /** @group Optics */
    val diffuser: Lens[Gcal, GcalDiffuser] =
      Focus[Gcal](_.diffuser)

    /** @group Optics */
    val shutter: Lens[Gcal, GcalShutter] =
      Focus[Gcal](_.shutter)
  end Gcal

  case object Science extends StepConfig(StepType.Science)

  final case class SmartGcal(
    smartGcalType: SmartGcalType,
  ) extends StepConfig(StepType.SmartGcal) derives Eq

  object SmartGcal:

    /** @group Optics */
    val smartGcalType: Lens[SmartGcal, SmartGcalType] =
      Focus[SmartGcal](_.smartGcalType)

  given Eq[StepConfig] = Eq.instance {
    case (Bias, Bias)                                 => true
    case (Dark, Dark)                                 => true
    case (a @ Gcal(_, _, _, _), b @ Gcal(_, _, _, _)) => a === b
    case (Science, Science)                           => true
    case (a @ SmartGcal(_), b @ SmartGcal(_))         => a === b
    case _                                            => false
  }

  /** @group Optics */
  val gcal: Prism[StepConfig, Gcal] =
    GenPrism[StepConfig, Gcal]

  /** @group Optics */
  val smartGcal: Prism[StepConfig, SmartGcal] =
    GenPrism[StepConfig, SmartGcal]