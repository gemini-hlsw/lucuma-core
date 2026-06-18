// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import cats.*
import cats.free.Free
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.model.Group
import lucuma.core.model.Observation

/**
 * To handle groups we unfold the flattened structure we get from the database into
 * a branching tree annotated by `minimumRequired` at each node and terminating with
 * values of arbitrary type, initially ids but this can be mapped to ItacObservation
 * and then flattened and scaled to a list of ItacObservation.
 */

case class ItacGroup[A](minimumRequired: Option[NonNegShort], children: List[A]):
  
  def scaleFactor: BigDecimal = 
    val count = children.length
    val min = minimumRequired.map(nns => Math.min(nns.value, count)) // min <= count
    min match
      case Some(m) => m.toDouble / count.toDouble
      case None    => 1.0
    
object ItacGroup:
  given Functor[ItacGroup] with
    def map[A, B](fa: ItacGroup[A])(f: A => B): ItacGroup[B] =
      fa.copy(children = fa.children.map(f))

type GroupTree[A] = Free[ItacGroup, A]
object GroupTree:

  def empty[A]: GroupTree[A] =
    fromList(Nil)

  def from[A](as: A*): GroupTree[A] =
    fromList(as.toList)

  def fromList[A](as: List[A]): GroupTree[A] =
    Free.roll(ItacGroup(None, as.map(Free.pure)))

  enum Item:
    def parent: Option[Group.Id]
    case Node(id: Group.Id, parent: Option[Group.Id], minimumRequired: Option[NonNegShort])
    case Leaf(id: Observation.Id, parent: Option[Group.Id])

  /** Unfold a flat list of items into a tree. */
  def unfold(items: List[Item]): GroupTree[Observation.Id] =
    def go(item: Item): GroupTree[Observation.Id] =
      item match
        case Item.Leaf(id, _) => Free.pure(id)
        case Item.Node(id, _, min) => Free.roll(ItacGroup(min, items.filter(_.parent == Some(id)).map(go)))
    Free.roll(ItacGroup(None, items.filter(_.parent.isEmpty).map(go)))

  /** Another generic fold to extract the leaves. */
  extension [F[_]: Functor, A, B] (self: Free[F, A]) def cata(ab: A => B, bb: F[B] => B): B =
    self.fold(ab, fa => bb(fa.map(_.cata(ab, bb))))

  /** Flatten a tree of observations into a list of observations scaled based on group membership.*/
  extension (self: GroupTree[ItacObservation]) def flattenAndScale: List[ItacObservation] =
    self.cata(List(_), g => g.children.flatten.map { o => o.copy(time = o.time *| g.scaleFactor) })
    
  extension[A] (self: GroupTree[A]) def dump(): Unit =
    def go(t: GroupTree[A], indent: Int): Unit =
      print(" " * indent)
      t.fold(
        println,
        t => 
          println(s"Group ${t.minimumRequired.fold("(all)")(n => s"($n of ${t.children.count})")}")
          t.children.foreach(go(_, indent + 2))
      )
    go(self, 0)
