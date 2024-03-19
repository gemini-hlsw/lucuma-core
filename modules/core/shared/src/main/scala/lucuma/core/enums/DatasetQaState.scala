// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for dataset QA state.
 * @group Enumerations
 */
sealed abstract class DatasetQaState(
  val tag:  String
) extends Product with Serializable {

  def shortName: String =
    tag

  def longName: String =
    tag

}

object DatasetQaState {

  /** @group Constructors */ case object Pass   extends DatasetQaState("Pass")
  /** @group Constructors */ case object Usable extends DatasetQaState("Usable")
  /** @group Constructors */ case object Fail   extends DatasetQaState("Fail")

  /** All members of DatasetQaState, in canonical order. */
  lazy val all: List[DatasetQaState] =
    List(Pass, Usable, Fail)

  /** Select the member of DatasetQaState with the given tag, if any. */
  def fromTag(s: String): Option[DatasetQaState] =
    all.find(_.tag === s)

  /** Select the member of DatasetQaState with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): DatasetQaState =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"DatasetQaState: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val EnumeratedDatasetQaState: Enumerated[DatasetQaState] =
    new Enumerated[DatasetQaState] {
      override def all: List[DatasetQaState] =
        DatasetQaState.all

      override def tag(a: DatasetQaState): String =
        a.tag

      override def unsafeFromTag(s: String): DatasetQaState =
        DatasetQaState.unsafeFromTag(s)
    }

}
